;;; hog.el --- Functions for working with Hog -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2023 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/hog-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools vhdl fpga
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; This extension facilitates the use of the FPGA build system Hog:
;;
;; https://hog.readthedocs.io

;;; Code:

;; TODO: parsing of ise projects
;; TODO: don't just dumbly error if the project isn't found
;; TODO: open should only find existing projects with XMLs
;; TODO: add a create-and-open command

(require 'xml)
(require 'subr-x)
(require 'json)
(require 'cl-lib)
(require 'thingatpt)

(defvar hog-vivado-path "/opt/Xilinx/Vivado/2021.1"
  "Path to the Xilinx Vivado installation.
Can be set in dir-locals to be changed on a per-project basis.")

(defvar hog-number-of-jobs 4
  "Number of jobs to use in Hog builds.")

(defvar hog-template-cache-dir user-emacs-directory
  "Directory in which to cache Vivado templates.")

(defun hog--check-for-vivado ()
  "Check to see if vivado exists at the specified path."
  (when (not (file-exists-p hog-vivado-path))
    (error (concat "Vivado not found at " hog-vivado-path))))

(defun hog--project-root ()
  "Get the root of the current version controlled project."
  (let ((submodule-parent
         (string-trim-right
          (shell-command-to-string
           "git rev-parse --show-superproject-working-tree")))
        (repo (string-trim-right
               (shell-command-to-string
                "git rev-parse --show-toplevel"))))

    (let ((repo-root
           (if (not  (string= submodule-parent ""))
               submodule-parent repo)))
      (if (not (string= repo-root ""))
          (concat repo-root "/") ""))))

(defun hog--get-projects ()
  "Get a list of available Hog projects."
  ;; convert the full directory into the path, e.g.
  ;; /home/topham/project/Top/myproject --> myproject

  ;; list all directories in the Top/ folder
  (let ((hog-top-folder (concat (hog--project-root) "Top")))
    (when (file-directory-p hog-top-folder)
      (sort (thread-last (shell-command-to-string (format "find %s -name hog.conf -type f" hog-top-folder))
                         (replace-regexp-in-string (concat  hog-top-folder "/") "")
                         (replace-regexp-in-string "/hog.conf" "")
                         (split-string)) #'string<))))

(defun hog--get-project-xml (project)
  "Return the XML (XPR) file for a given Hog PROJECT."
  (let* ((base  (format "%sProjects/%s/%s" (hog--project-root) project project))
         (xpr (format "%s.xpr" base))
         (ppr (format "%s.ppr" base)))
    (cond
     ((file-exists-p xpr) xpr)
     ((file-exists-p ppr) ppr)
     (t (error (format  "Project %s XML not found!" project))))))

(defmacro hog--project-do! (name docstring body)
  "Macro to create an arbitrary Hog interactive command.

NAME is the function name

DOCSTRING will be the DOCSTRING of the generated function

BODY is the body of the command that should be executed"
  `(defun ,name (project)
     ,docstring
     (interactive (list (completing-read "Project: " (hog--get-projects) nil t)))
     (if (not (string-empty-p project))
         (eval ,body)
       (message "You must specify a valid project!"))))

(defmacro hog--create-command! (name command docstring)
  "Macro to create a Hog interactive command.
NAME is the function name, COMMAND is the command that should be
executed, and DOCSTRING will be passed into the generated function."
  `(defun ,name (project)
     ,docstring
     (interactive (list (completing-read "Project: "
                                         (hog--get-projects)
                                         nil
                                         t)))
     (if (not (string-empty-p project))
         (progn (hog--run-command ,command project))
       (message "You must specify a valid project!"))))

(hog--create-command! hog-create-project "Hog/CreateProject.sh" "Create a Hog project")
(hog--create-command! hog-launch-synthesis (format "Hog/LaunchWorkflow.sh -synth_only -njobs %d" hog-number-of-jobs) "Launch Project Synthesis")
(hog--create-command! hog-launch-impl (format "Hog/LaunchWorkflow.sh -impl_only -njobs %d" hog-number-of-jobs) "Launch Project Implementation")
(hog--create-command! hog-launch-workflow (format "Hog/LaunchWorkflow.sh -njobs %d" hog-number-of-jobs) "Launch Project Full Workflow")

;; TODO: check if the xml file exists, prompt to create if it doesn't
(hog--project-do!
 hog-open-project
 "Open the Hog PROJECT."
 (progn
   (hog--check-for-vivado)
   (let ((project-file (hog--get-project-xml project)))
     (if (and project-file (file-exists-p project-file))
         (progn
           (let ((command (format "cd %s && source %s && vivado %s &"
                                  (hog--project-root)
                                  (concat  hog-vivado-path "/settings64.sh")
                                  project-file)))
             (message (format "Opening Hog Project %s" project))
             (start-process "*vivado*" nil "setsid" command)))
       (message (format "Project file %s not found!" project-file))))))

(defun hog--run-command (command project &rest args)
  "Run a Hog COMMAND for a given PROJECT.

colorize it using CCZE, with the Hog arguments ARGS."

  (hog--check-for-vivado)
  (let* ((name (format "%s" command))
         (buf (format "*%s*" name)))

    ;; construct the output command
    (let ((cmd-str (format "cd %s && source %s && %s | tee hog.log %s"
                           (hog--project-root) ;; cd %s
                           (concat  hog-vivado-path "/settings64.sh") ;; source vivado
                           (concat
                            ;; path/Hog/Launch{X}.sh project <args>
                            (hog--project-root) command " " project " " (string-join args " "))
                           ;; optional ccze
                           (if (executable-find "ccze") " | ccze -A" ""))))
      ;; ... and run it
      (compile cmd-str))

    ;; change the output buffer to a read-only, evil normal state buffer
    (with-current-buffer buf (view-mode))))

;;--------------------------------------------------------------------------------
;; Intelligence for reading project xpr/ppr files
;;--------------------------------------------------------------------------------

(defun hog--read-lines-from-file (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun hog--parse-vivado-xpr (project-file)
  "Parse a Vivado XPR PROJECT-FILE into a list of libraries and their sources."
  (let ((lib-list (list)))
    (dolist (file-node
             ;; get a list of all the Project -> FileSets -> FileSet --> File nodes
             (xml-get-children (assq 'FileSet (assq 'FileSets (assq 'Project (xml-parse-file project-file)))) 'File))
      ;; for each node, extract the path to the .src file
      (let ((src-file
             ;; strip off the vivado relative path; make it relative to the repo root instead
             ;; (regexp-quote "$PPRDIR/../../")
             (replace-regexp-in-string "$PPRDIR\/\.\.\/\.\.\/" "" (xml-get-attribute file-node 'Path ))))
        ;; for each node, extract the library property (only applies to vhdl sources)
        (dolist (attr (xml-get-children (assq 'FileInfo (cdr file-node)) 'Attr))
          (when (equal (xml-get-attribute attr 'Name) "Library")
            (let ((lib  (xml-get-attribute attr 'Val)))
              (setf lib-list (hog--append-to-library lib-list lib src-file)))))))
    lib-list))

(defun hog--parse-ise-ppr (project-file)
  "Parse an ISE PPR PROJECT-FILE.

Parses the PPR file into a list of libraries and their sources."
  ;; FIXME need to parse the dang thing
  project-file)

(defun hog--parse-xml (xml)
  "Parse a Xilinx XML file into a list.

The resulting list is of the form:

((library1 (file1 file2 file3))
 (library2 (file1 file2 file3)))"

  (let* ((extension (file-name-extension xml)))
    (cond ((string-equal extension "xpr")
           (hog--parse-vivado-xpr xml))
          ((string-equal extension "ppr")
           (hog--parse-ise-ppr xml)))))

(defun hog--parse-project-xml (project)
  "Parse a PROJECT xml file into a list."
  (hog--parse-xml (hog--get-project-xml project)))

(defun hog--append-to-library (src-list lib-name file-name)
  "SRC-LIST LIB-NAME FILE-NAME."
  (let ((lib (assoc lib-name src-list)))
    (when (eq lib nil)
      (setf src-list (append src-list (list (list lib-name (list)))))
      (setq lib (assoc lib-name src-list)))
    (setf (cadr lib) (append (cadr lib) (list file-name) )))
  src-list)

(defvar hog-ieee-library
  '("ieee" (
            "/usr/local/lib/ghdl/src/synopsys/*.vhdl"
            "/usr/local/lib/ghdl/src/std/v08/*.vhdl"
            "/usr/local/lib/ghdl/src/ieee2008/*.vhdl"
            "/usr/lib/ghdl/src/synopsys/*.vhdl"
            "/usr/lib/ghdl/src/std/v08/*.vhdl"
            "/usr/lib/ghdl/src/ieee2008/*.vhdl")))

(defvar hog-unisim-library
  (list "unisim"
        (list (format "%s/data/vhdl/src/unisims/unisim_VCOMP.vhd" hog-vivado-path))))

;;------------------------------------------------------------------------------
;; VHDL Tool YAML Config Generation
;;------------------------------------------------------------------------------

(defvar hog-vhdl-tool-preferences
  '(("TypeCheck"            . t)
    ("MultiLineErrors"      . t)
    ("CheckOnChange"        . t)
    ("Lint"                 . t)
    ("FirstSyntaxErrorOnly" . t)))

(defvar hog-vhdl-tool-lint-settings
  '(("Threshold" ."Warning")
    ("DeclaredNotAssigned" .
     (("enabled"  . t)
      ("severity" . "Warning")))
    ("DeclaredNotRead"           . t)
    ("ReadNotAssigned"           . t)
    ("SensitivityListCheck"      . t)
    ("ExtraSensitivityListCheck" . t)
    ("DuplicateSensitivity"      . t)
    ("LatchCheck"                . t)
    ("VariableNotRead"           . t)
    ("PortNotRead"               . t)
    ("PortNotWritten"            . t)
    ("NoPrimaryUnit"             . t)
    ("DuplicateLibraryImport"    . t)
    ("DuplicatePackageUsage"     . t)
    ("DeprecatedPackages"        . t)
    ("ImplicitLibraries"         . t)
    ("DisconnectedPorts"         . t)
    ("IntNoRange"                . t)))

(hog--project-do!
 hog-vhdl-tool-create-project-yaml
 "Create a VHDL-tool yaml file for a Hog PROJECT"
 (with-temp-file (format "%s/vhdltool-config.yaml" (hog--project-root))
   (progn
     (insert
      (json-encode
       (list (cons 'Libraries
                   (mapcar (lambda (lib)
                             (list (cons 'name (car lib))
                                   (cons 'paths (apply #'vector (cadr lib)))))
                           (append
                            (hog--parse-project-xml project)
                            (list hog-ieee-library)
                            (list hog-unisim-library))))
             (cons 'Preferences
                   hog-vhdl-tool-preferences)
             (cons 'Lint
                   hog-vhdl-tool-lint-settings)))))
   (json-pretty-print-buffer)))

;;------------------------------------------------------------------------------
;; VHDL LS TOML Project File Creation
;;------------------------------------------------------------------------------

(defun hog--vhdl-ls-lib-to-string (library)
  "LIBRARY."
  (let ((lib-name (car library))
        (lib-files (car (cdr library)))
        (pad "  "))

    (format "\n%s.files = [\n%s]\n"
            lib-name
            (string-join (mapcar (lambda (file)
                                   (concat pad "\"" file "\",\n" )) lib-files)))))

(defun hog--vhdl-ls-parse-libs (libraries)
  "LIBRARIES."
  (let ((text "[libraries]\n"))
    (setq libraries (append libraries (list hog-ieee-library)))
    (setq libraries (append libraries (list hog-unisim-library)))
    (dolist (library libraries)
      ;;(concat text (hog--vhdl-ls-lib-to-string library))
      ;;(print (concat text (hog--vhdl-ls-lib-to-string library)))
      (setq text (concat text (hog--vhdl-ls-lib-to-string library))))
    text))

(hog--project-do!
 hog-vhdl-ls-create-project-toml
 "Create a VHDL-ls yaml file for a Hog PROJECT"
 (let ((yaml (hog--vhdl-ls-parse-libs (hog--parse-project-xml project))))
   (shell-command (format "echo '%s' > %svhdl_ls.toml" yaml (hog--project-root)))))

;;------------------------------------------------------------------------------
;; GHDL-LS JSON Project File Creation
;;------------------------------------------------------------------------------

(defvar hog--ghdl-ls-options
  (let ((options (make-hash-table)))
    (puthash 'ghdl_analysis
             '("--workdir=work"
               "--ieee=synopsys"
               "-fexplicit"
               "--warn-library"
               "--warn-default-binding"
               "--warn-binding"
               "--warn-reserved"
               "--warn-nested-comment"
               "--warn-parenthesis"
               "--warn-vital-generic"
               "--warn-delayed-checks"
               "--warn-body"
               "--warn-specs"
               "--warn-runtime-error"
               "--warn-shared"
               "--warn-hide"
               "--warn-unused"
               "--warn-others"
               "--warn-pure"
               "--warn-static"
               "--std=08"
               "-fexplicit")
             options) options))

(defun hog--ghdl-ls-format-lib (lib)
  ""
  (let ((libname (car lib))
        (libfiles (cadr lib)))
    (cl-flet ((format-lib-file (file)
                               (list `(file . ,file)
                                     '(language . "vhdl")
                                     `(library . ,libname))))
      (mapcar #'format-lib-file libfiles))))

(defun hog--ghdl-ls-format-libs (libs)
  ""
  (apply #'append
         (mapcar #'hog--ghdl-ls-format-lib libs)))

(defun hog--ghdl-ls-make-config (libs)
  ""
  (let ((config (make-hash-table)))
    (puthash 'options hog--ghdl-ls-options config)
    (puthash 'files (hog--ghdl-ls-format-libs libs) config)
    config))

(hog--project-do!
 hog-ghdl-ls-create-project-json
 "Create GHDL-LS Json File"
 (if (string-equal project "")
     (message "You must specify a valid project!")
   (let ((output-file (format "%shdl-prj.json" (hog--project-root)))
         (config (hog--ghdl-ls-make-config
                  (append (list hog-unisim-library)
                          (hog--parse-project-xml project)))))
     (with-temp-file output-file
       (progn (insert (json-encode config))
              (json-pretty-print-buffer))))))

;;------------------------------------------------------------------------------
;; Hog Source File Mode
;;------------------------------------------------------------------------------

(defvar hog--src-property-re
  (rx  (seq (one-or-more " ")
            (group (or "lib" "top"))
            "="
            (group (one-or-more nonl)))))

(defvar hog--file-name-re
  (rx (seq
       (? "#")                                             ; optional comment
       (submatch (* (any alphanumeric ?* ?_ ?- ?/ ?.)))    ; file path
       (? (+ " "))                                         ; optional whitespace
       (submatch (* (any alphanumeric ?* ?_ ?- ?/ ?.)))))) ; file path

(defvar hog--src-symbols-list
  '("locked" "93" "nosynth" "noimpl" "nosim" "source" "SystemVerilog"
    "verilog_header" "XDC"))

(defun hog--get-link-at-point ()
  "Return the link at point in a Hog src file."
  (let ((inhibit-message t))
    (let ((filename
           (progn
             (thing-at-point-looking-at hog--file-name-re)
             (match-string-no-properties 1))))
      (concat (hog--project-root)
              filename))))

;; FIXME: does not work with file names with spaces
;; spaces should be escaped
(defun hog-follow-link-at-point ()
  "Follow the Hog source file at point."
  (interactive)
  ;; TODO: check if the file contains a wildcard suffix, if so open it as a directory
  (let ((file-with-path (hog--get-link-at-point)))
    (when (file-exists-p file-with-path)
      (find-file file-with-path))))

(defun hog-expand-glob-at-point ()
  "Unglob a globbed entry in a source file.
When pointed at a globbed (wildcard) in a source file, this
function will unglob it and insert the explicit list of all files
in that path"
  (interactive)
  (save-excursion
    (let* (;; Get the thing the cursor is looking at
           (filename (progn (thing-at-point-looking-at hog--file-name-re)
                            (match-string-no-properties 1)))

           ;; Concat the absolute path onto the file name which is relative to
           ;; the root of the repo
           (file-with-path (concat (hog--project-root) filename))

           ;; Expand the absolute path
           (files (file-expand-wildcards file-with-path nil))

           ;; Converted the expanded absolute file names back to relative ones
           (files-relative (mapcar (lambda (x) (replace-regexp-in-string (hog--project-root) "" x)) files))

           ;; Concate everything together into a string
           (files-expanded (string-join files-relative "\n")))

      (end-of-line)
      (newline)
      (insert files-expanded))))

(defvar hog-src-mode-map (make-sparse-keymap)
  "Keymap for `hog-src-mode'.")

(define-key hog-src-mode-map (kbd "M-RET") #'hog-follow-link-at-point)

(define-generic-mode 'hog-src-mode
  ;; comment list
  '("#")

  ;; keyword list
  nil

  ;; font lock list:
  `((,(regexp-opt hog--src-symbols-list 'symbols) . font-lock-keyword-face) ; symbol highlighting
    (,hog--src-property-re . (1  font-lock-keyword-face))                   ; lib=
    (,hog--src-property-re . (2  font-lock-builtin-face))                   ; =ieee
    (,hog--file-name-re . (1  font-lock-string-face))                       ; file name
    (,hog--file-name-re . (2  font-lock-doc-face)))                         ; ipbus decoder file

  ;; auto mode list
  '("\\.src\\'" "\\.con\\'" "\\.lst\\'")

  ;; function list
  (list
   (lambda ()
     ;; (modify-syntax-entry ?= ".")
     (use-local-map hog-src-mode-map)))

  ;; docstring
  "Major mode for Hog src files")

(defun hog-clean-vivado-xci ()
  "Clean the output products of Vivado XCI Files."
  (interactive)

  (let ((file-count 0)
        (file-size 0))

    (let ((xci-files
           (split-string
            (shell-command-to-string
             (concat  "find " (hog--project-root) " -name *.xci")) "\n" t)))
      (dolist (xci xci-files)
        (let ((file-name-noext (file-name-sans-extension xci))
              (dirname (file-name-directory xci)))

          (let ((files-to-remove
                 (append
                  (mapcar (lambda (x) (concat file-name-noext x))
                          '(".dcp" ".veo" ".vho" ".xml" "_sim_netlist.v"
                            "_ooc.xdc" "_sim_netlist.vhdl" "_stub.v" "_stub.vhdl")))))
            (dolist (file files-to-remove)
              (when (file-exists-p file)
                (setq file-count (+ 1 file-count))
                (setq file-size (+ (file-attribute-size (file-attributes file)) file-size))
                (princ (format "Removing %s\n" file))
                (delete-file file))))

          (let ((directories-to-remove
                 (append (mapcar (lambda (x) (concat dirname x))
                                 '("hdl" "synth" "doc" "sim" "ila_v6_2")))))
            (dolist (file directories-to-remove)
              (when (file-exists-p file)
                (setq file-count (+ (string-to-number (shell-command-to-string (concat  "find " file " | wc -l"))) file-count))
                (setq file-size (+ (string-to-number (car (split-string  (shell-command-to-string (concat  "du " file))))) file-size))
                (princ (format "Removing %s\n" file))
                (delete-directory file t)))))))

    (princ (format  "Removed %d files, %s"
                    file-count
                    (file-size-human-readable file-size)))))

;;------------------------------------------------------------------------------
;; Vivado Template Insertion
;;------------------------------------------------------------------------------

(cl-defun hog--vivado-collect-templates (nodes &key components parents)
  "Collect a list of all Vivado templates from xml NODES.
It is called recursively, tracking the collective COMPONENTS and
the hierarchy of their PARENTS."

  (let* ((children (xml-node-children nodes)))

    (dolist (child children)
      (when (listp child)
        (let* ((child-name (xml-get-attribute child 'label))
               (tree-type (xml-get-attribute child 'treetype))
               (template-path
                (if (string=  tree-type "template")
                    (cons child-name parents) nil)))

          (when template-path
            (push (reverse template-path) components))

          (let ((sub-components
                 (hog--vivado-collect-templates child
                                                :components components
                                                :parents (cons child-name parents))))
            (when sub-components
              (setq components sub-components)))))))
  components)

(defun hog--walk-vivado-template-xml (FILE)
  "Walks through a vivado XML template FILE and collects templates."
  (hog--vivado-collect-templates
   (assq 'RootFolder (xml-parse-file FILE))))

(defun hog--stringify-templates (templates)
  "Stringifies a list of TEMPLATES.
It joins together the path into a single string with separated by arrows."
  (mapcar (lambda (x) (string-join x " -> ")) templates))

(defun hog--template-cache (lang)
  "Return the path of the hog cached template list for a given LANG."
  (concat hog-template-cache-dir "/" (symbol-name lang) ".json"))

(defun hog--template-xml-path (lang)
  "Return the path of the vivado xml template file for a given LANG."
  (concat hog-vivado-path
          "/data/parts/xilinx/templates/vivado/"
          "/" (symbol-name lang) ".xml"))

(defun hog--get-templates (lang)
  "Return the template list for a given LANG.
This uses either cached values stored in JSON, or creating the
JSON file if it does not exist."

  ;; if the cache file does not exist, create it
  (when (not  (file-exists-p (hog--template-cache lang)))
    (with-temp-file (hog--template-cache lang)
      (insert (json-encode
               (cons "Templates" (hog--walk-vivado-template-xml
                                  (hog--template-xml-path lang)))))))

  ;; else read the cache file
  (let ((json-array-type 'list))
    (cdr (json-read-file (hog--template-cache lang)))))

(defun hog--vivado-decend-template (nodes path)
  "Return a single Vivado template text.
This walks through a collection of XML NODES, and finds the
template at a specific PATH."

  (let ((children (xml-node-children nodes))
        (template-content nil))
    (dolist (child children)
      (when path
        (when (listp child)
          (let ((child-name (xml-get-attribute child 'label))
                (treetype (xml-get-attribute child 'treetype)))
            (when (string= child-name (car path))
              (setq template-content
                    (if (not (string= treetype "template"))
                        (hog--vivado-decend-template child (cdr path))
                      (car (last child)))))))))
    template-content))

(defun hog--insert-template (lang)
  "Insert a vivado template for a specific LANG."

  (let ((template
         (completing-read
          "Template: "
          (hog--stringify-templates (hog--get-templates lang)))))

    (message (concat "Inserting: " template))
    (let ((template-text
           (hog--vivado-decend-template
            (assq 'RootFolder (xml-parse-file (hog--template-xml-path lang)))
            (split-string template " -> "))))

      ;; replace trailing tabs and insert the template
      (insert (replace-regexp-in-string "[[:blank:]]*$" "" template-text)))))

(defun hog-insert-vhdl-template ()
  "Insert a vivado vhdl template."
  (interactive (hog--insert-template 'vhdl)))

(defun hog-insert-verilog-template ()
  "Insert a vivado verilog template."
  (interactive (hog--insert-template 'verilog)))

(defun hog-insert-xdc-template ()
  "Insert a vivado XDC template."
  (interactive (hog--insert-template 'xdc)))

(defun hog-insert-systemverilog-template ()
  "Insert a vivado systemverilog template."
  (interactive (hog--insert-template 'systemverilog)))

(defun hog-check-src-file ()
  "Check a Hog source file for broken links."
  (let ((errors 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (line-number-at-pos) (line-number-at-pos (point-max)))

        ;; skip comments
        (when (not (string-match "^\s*#.*"
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))

          (let* ((link (hog--get-link-at-point))
                 (link-is-glob (file-expand-wildcards link))
                 (link-exists (file-exists-p link)))

            (when (not (or link-exists link-is-glob))
              (setq errors (+ 1 errors))
              (princ (format "Error:%d %s not found\n" (line-number-at-pos) link)))))
        (forward-line))) errors))

(provide 'hog)
;;; hog.el ends here
;; LocalWords:  xml vivado vhdl verilog systemverilog stringifies globbed unglob SRC
