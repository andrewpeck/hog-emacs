;;; hog.el --- Functions for working with Hog -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2022 Andrew Peck

;; Author: Andrew Peck <andrew.peck@cern.ch>
;; URL: https://github.com/andrewpeck/hog-emacs
;; Version: 0.0.0
;; Package-Requires: ((projectile "2.2") (emacs "24.4"))
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
;; This extension facilitates the use of the FPGA build system Hog
;; https://hog.readthedocs.io

;;; Code:

;; TODO: parsing of ise projects
;; TODO: don't just dumbly error if the project isn't found
;; TODO: open should only find existing projects with XMLs
;; TODO: add a create-and-open command

(require 'xml)
(require 'json)
(require 'projectile)

(defvar hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
(defvar hog-number-of-jobs 4)

(defun hog--get-projects ()
  "Get a list of available Hog projects."
  ;; convert the full directory into the path, e.g.
  ;; /home/topham/project/Top/myproject --> myproject
  (mapcar (lambda (file) (file-name-nondirectory (directory-file-name
                                                  (expand-file-name (concat file "/..")))))
          ;; list all directories in the Top/ folder
          (if (file-directory-p (format "%sTop" (projectile-project-root)))
              (sort (split-string
                     (shell-command-to-string
                      (format
                       "find %sTop -name hog.conf -type f -or -name *.src -type f"
                       (projectile-project-root)))) #'string<))))

(defun hog--get-project-xml (project)
  "Return the XML (XPR) file for a given Hog PROJECT."
  (let* ((base  (format "%sProjects/%s/%s" (projectile-project-root) project project))
         (xpr (format "%s.xpr" base))
         (ppr (format "%s.ppr" base)))
    (cond
     ((file-exists-p xpr) xpr)
     ((file-exists-p ppr) ppr))))

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

;; TODO: open projects based on found xml files rather than Top files
(hog--project-do!
 hog-open-project
 "Open the Hog PROJECT."
 (progn
   (let ((command (format "cd %s && source %s && vivado %s &"
                          (projectile-project-root)
                          hog-vivado-path
                          (hog--get-project-xml project))))
     (message (format "Opening Hog Project %s" project))
     (async-shell-command command))))

(defun hog--run-command (command project &rest args)
  "Run a Hog COMMAND for a given PROJECT.

colorize it using CCZE, with the Hog arguments ARGS."

  (let* ((name (format "%s" command))
         (buf (format "*%s*" name)))

    ;; construct the output command
    (let ((cmd-str (format "cd %s && source %s && %s | tee hog.log %s"
                           (projectile-project-root) ;; cd %s
                           hog-vivado-path           ;; source vivado
                           (concat
                            ;; path/Hog/Launch{X}.sh project <args>
                            (projectile-project-root) command " " project " " (string-join args " "))
                           ;; optional ccze
                           (if (executable-find "ccze") " | ccze -A" ""))))
      ;; ... and run it
      (compile cmd-str buf))

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

(defun hog--parse-project-xml (project)
  "Parse a PROJECT xml file into a list."
  (let* ((xml (hog--get-project-xml project))
         (extension (file-name-extension xml)))
    (cond ((string-equal extension "xpr")
           (hog--parse-vivado-xpr xml))
          ((string-equal extension "ppr")
           (hog--parse-ise-ppr xml)))))

(defun hog--append-to-library (src-list lib-name file-name)
  "SRC-LIST LIB-NAME FILE-NAME."
  (let ((lib (assoc lib-name src-list)))
    (when (eq lib nil)
      (setf src-list (append src-list (list (list lib-name (list)))))
      ;;(print src-list)
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
  `("unisim" (
              ,(format "%sdata/vhdl/src/unisims/unisim_VCOMP.vhd"
                       (file-name-directory hog-vivado-path)))))

;;------------------------------------------------------------------------------
;; VHDL Tool YAML Config Generation
;;------------------------------------------------------------------------------

(defvar hog-vhdl-tool-preferences
  '(
    ("TypeCheck"            . t)
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
 (with-temp-file (format "%s/vhdltool-config.yaml" (projectile-project-root))
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
    (concat
     (format "%s.files = [\n" lib-name)
     (string-join (mapcar (lambda (file)
                            (concat pad "\"" file "\",\n" )) lib-files))
     "]\n")))

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
 "Create a VHDL-tool yaml file for a Hog PROJECT"
 (let ((yaml (hog--vhdl-ls-parse-libs (hog--parse-project-xml project))))
   (shell-command (format "echo '%s' > %svhdl_ls.toml" yaml (projectile-project-root)))))

;;------------------------------------------------------------------------------
;; GHDL-LS JSON Project File Creation
;;------------------------------------------------------------------------------

(defvar hog--ghdl-ls-options
  '(options
    (ghdl_analysis .
                   ["--workdir=work"
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
                    "-fexplicit"])))

(defun hog--ghdl-ls-format-file-list (file-list)
  "FILE-LIST."
  (list (cons 'files
              (mapcar
               (lambda (file-name) (list `(file . ,file-name) '(language . "vhdl")))
               file-list))))

(hog--project-do!
 hog-ghdl-ls-create-project-json
 "Create GHDL-LS Json File"
 (if (not (string-equal project ""))
     (progn (let ((output-file (format "%shdl-prj.json" (projectile-project-root)))
                  (files (apply #'append (mapcar #'cadr (hog--parse-project-xml project)))))
              (with-temp-file output-file
                (progn (insert (json-encode (append (list hog--ghdl-ls-options)
                                                    (hog--ghdl-ls-format-file-list files))))
                       (json-pretty-print-buffer)))))
   (message "You must specify a valid project!")))

;;------------------------------------------------------------------------------
;; Testing
;;------------------------------------------------------------------------------

(eval-when-compile
  (cl-flet
      ((check-lsp-output-file
        (func output)
        (when (funcall func "test")
          (rename-file output (format "test/%s" output) t)
          (let ((diff (shell-command-to-string (format "git diff test/%s" output))))
            (if (not (string-empty-p diff))
                (error (format "Diff in %s:\n%s" output diff)))))))

    ;; (let ((dir (file-name-directory load-file-name)))
    ;;   (cd dir)
    ;;   (check-lsp-output-file 'hog-ghdl-ls-create-project-json "hdl-prj.json")
    ;;   (check-lsp-output-file 'hog-vhdl-ls-create-project-toml "vhdl_ls.toml")
    ;;   (check-lsp-output-file 'hog-vhdl-tool-create-project-yaml "vhdltool-config.yaml")
    ;;   )))
    ))

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
       (? "#")                                          ; optional comment
       (submatch (* (any alphanumeric ?* ?_ ?- ?/ ?.))) ; file path
       (? (+ " "))                                      ; optional whitespace
       (submatch (* (any alphanumeric ?* ?_ ?- ?/ ?.))) ; file path
       )))

(defvar hog--src-symbols-list
  '("locked" "93" "nosynth" "noimpl" "nosim" "source" "SystemVerilog" "verilog_header" "XDC"))

;; FIXME: does not work with file names with spaces
;; spaces should be escaped
(defun hog-follow-link-at-point ()
  "Follow the Hog source file at point."
  (interactive)
  (save-excursion
    (let ((filename
           (progn
             (thing-at-point-looking-at hog--file-name-re)
             (match-string-no-properties 1))))
      ;;  probably shouldn't open if its a normal link, use link-hint-open-link
      ;;  else
      (find-file
       (concat (projectile-project-root) filename)))))

(defvar hog-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'hog-follow-link-at-point)
    map)
  "Keymap for `hog-src-mode'.")

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

     ;; The syntax is changed only for table SYNTAX-TABLE, which defaults to
     ;; the current buffer's syntax table.
     ;; CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
     ;; in the range MIN to MAX are changed.
     ;;
     ;;  Space or -  whitespace syntax.    w   word constituent.
     ;;  _           symbol constituent.   .   punctuation.
     ;;  (           open-parenthesis.     )   close-parenthesis.
     ;;  "           string quote.         \   escape.
     ;;  $           paired delimiter.     '   expression quote or prefix operator.
     ;;  <           comment starter.      >   comment ender.
     ;;  /           character-quote.      @   inherit from parent table.
     ;;  |           generic string fence. !   generic comment fence.

     ;; Treat these characters as punctuation, meaning that
     ;; e.g. "|KEYWORD" is treated similarly to "KEYWORD".
     (modify-syntax-entry ?= ".")

     (use-local-map hog-src-mode-map)))

  ;; docstring
  "Major mode for Hog src files")

(provide 'hog)
;;; hog.el ends here
