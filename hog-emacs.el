;;; hog-emacs.el --- functions for working with Hog -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; License:

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

;;; Code:

(require 'xml)

(defvar hog-vivado-path "~/Xilinx/Vivado/2020.2/settings64.sh")
(defvar hog-number-of-jobs 4)

(defun hog-get-projects ()
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
                       "find %sTop -name list -type d -or -name hog.conf -type f -or -name *.src -type f"
                       (projectile-project-root)))) #'string<))))

(defun hog-get-project-xml (project)
  "Return the XML (XPR) file for a given Hog PROJECT."
  (let* ((base  (format "%sProjects/%s/%s" (projectile-project-root) project project))
         (xpr (format "%s.xpr" base))
         (ppr (format "%s.ppr" base)))
    (cond
     ((file-exists-p xpr) xpr)
     ((file-exists-p ppr) ppr))))

;;;###autoload (autoload 'hog-project-do! "hog-emacs")
(defmacro hog-project-do! (name docstring body)
  "Macro to create an arbitrary Hog interactive command.
NAME is the function name, COMMAND is the command that should be executed"
  `(defun ,name (project)
     ,docstring
     (interactive (list (completing-read "Project: " (hog-get-projects) nil t)))
     (if (not (string-empty-p project))
         (eval ,body)
       (message "You must specify a valid project!"))))

;;;###autoload (autoload 'hog-create-command! "hog-emacs")
(defmacro hog-create-command! (name command docstring)
  "Macro to create a Hog interactive command.
NAME is the function name, COMMAND is the command that should be executed"
  `(defun ,name (project)
     ,docstring
     (interactive (list (completing-read "Project: "
                                         (hog-get-projects)
                                         nil
                                         t)))
     (if (not (string-empty-p project))
         (progn (hog-run-command ,command project))
       (message "You must specify a valid project!"))))

(hog-create-command! hog-create-project "Hog/CreateProject.sh" "Create a Hog project")
(hog-create-command! hog-launch-synthesis (format "Hog/LaunchWorkflow.sh -synth_only -njobs %d" hog-number-of-jobs) "Launch Project Synthesis")
(hog-create-command! hog-launch-impl (format "Hog/LaunchWorkflow.sh -impl_only -njobs %d" hog-number-of-jobs) "Launch Project Implementation")
(hog-create-command! hog-launch-workflow (format "Hog/LaunchWorkflow.sh -njobs %d" hog-number-of-jobs) "Launch Project Full Workflow")

;;;###autoload
(hog-project-do!
 hog-open-project
 "Open the Hog PROJECT."
 (progn
   (let ((command (format "cd %s && source %s && vivado %s &"
                          (projectile-project-root)
                          hog-vivado-path
                          (hog-get-project-xml project)
                          )))
     (message (format "Opening Hog Project %s" project))
     (async-shell-command command))))

(defun hog-run-command (command project &rest args)
  "Run a Hog COMMAND for a given PROJECT (and colorize it)."

  (let* ((name (format "%s" command))
         (buf (format "*%s*" name)))

    ;; construct the output command
    (let ((cmd-str (format "cd %s && source %s && %s | tee hog.log %s"
                           (projectile-project-root) ;; cd %s
                           hog-vivado-path ;; source vivado
                           (concat
                            ;; path/Hog/Launch{X}.sh project <args>
                            (projectile-project-root) command " " project " " (string-join args " "))
                           ;; optional ccze
                           (if (executable-find "ccze") " | ccze -A" "")
                           )))
      ;; ... and run it
      (compile cmd-str buf))

    ;; change the output buffer to a read-only, evil normal state buffer
    (with-current-buffer buf (evil-normal-state) (view-mode))))

;;--------------------------------------------------------------------------------
;; Intelligence for reading project xpr/ppr files
;;--------------------------------------------------------------------------------

(defun hog-read-lines-from-file (file-path)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun hog-parse-vivado-xpr (project-file)
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
              (setf lib-list (hog-append-to-library lib-list lib src-file)))))))
    lib-list))

(defun hog-parse-ise-ppr (project-file)
  "Parse a Vivado PPR (ISE) PROJECT-FILE into a list of libraries and their sources."
  ;; FIXME need to parse the dang thing
  )

(defun hog-parse-project-xml (project)
  "Parse a PROJECT xml file into a list"
  (let* ((xml (hog-get-project-xml project))
         (extension (file-name-extension xml)))
    (cond ((string-equal extension "xpr")
           (hog-parse-vivado-xpr xml))
          ((string-equal extension "ppr")
           (hog-parse-ise-ppr xml)))))

(defun hog-append-to-library (src-list lib-name file-name)
  ""
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

;;;###autoload
(hog-project-do!
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
                            (hog-parse-project-xml project)
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

(defun hog-vhdl-ls-lib-to-string (library)
  ""
  (let ((lib-name (car library))
        (lib-files (car (cdr library)))
        (pad "  "))
    (concat
     (format "%s.files = [\n" lib-name)
     (string-join (mapcar (lambda (file)
                            (concat pad "\"" file "\",\n" )) lib-files))
     "]\n")))

(defun hog-vhdl-ls-parse-libs (libraries)
  ""
  (let ((text "[libraries]\n"))
    (setq libraries (append libraries (list hog-ieee-library)))
    (setq libraries (append libraries (list hog-unisim-library)))
    (dolist (library libraries)
      ;;(concat text (hog-vhdl-ls-lib-to-string library))
      ;;(print (concat text (hog-vhdl-ls-lib-to-string library)))
      (setq text (concat text (hog-vhdl-ls-lib-to-string library))))
    text))

;;;###autoload
(hog-project-do!
 hog-vhdl-ls-create-project-toml
 "Create a VHDL-tool yaml file for a Hog PROJECT"
 (let ((yaml (hog-vhdl-ls-parse-libs (hog-parse-project-xml project))))
   (shell-command (format "echo '%s' > %svhdl_ls.toml" yaml (projectile-project-root)))))

;;------------------------------------------------------------------------------
;; GHDL-LS JSON Project File Creation
;;------------------------------------------------------------------------------

(defvar ghdl-ls-options
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

(defun ghdl-ls-format-file-list (file-list)
  (list (cons 'files
              (mapcar
               (lambda (file-name) (list `(file . ,file-name) '(language . "vhdl")))
               file-list))))

;;;###autoload
(hog-project-do!
 hog-ghdl-ls-create-project-json
 "Create GHDL-LS Json File"
 (if (not (string-equal project ""))
     (progn (let ((output-file (format "%shdl-prj.json" (projectile-project-root)))
                  (files (apply #'append (mapcar #'cadr (hog-parse-project-xml project)))))
              (with-temp-file output-file
                (progn (insert (json-encode (append (list ghdl-ls-options) (ghdl-ls-format-file-list files))))
                       (json-pretty-print-buffer)))))
   (message "You must specify a valid project!")))

;;------------------------------------------------------------------------------
;; Testing
;;------------------------------------------------------------------------------

(eval-when-compile
  (require 'json)
  (cl-flet
      ((check-lsp-output-file
        (func output)
        (when (funcall func "test")
          (rename-file output (format "test/%s" output) t)
          (let ((diff (shell-command-to-string (format "git diff test/%s" output))))
            (if (not (string-empty-p diff))
                (error (format "Diff in %s:\n%s" output diff)))))))

    (check-lsp-output-file 'hog-ghdl-ls-create-project-json "hdl-prj.json")
    (check-lsp-output-file 'hog-vhdl-ls-create-project-toml "vhdl_ls.toml")
    (check-lsp-output-file 'hog-vhdl-tool-create-project-yaml "vhdltool-config.yaml")))

(provide 'hog-emacs)
;;; hog-emacs.el ends here
