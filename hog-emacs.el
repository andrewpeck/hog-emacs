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


;; TODO: add Unisim libraries

(defvar hog-vivado-path "~/Xilinx/Vivado/2019.2/settings64.sh")
(defvar hog-number-of-jobs 4)

(defun hog-get-projects ()
  "Get a list of available Hog projects."
  ;; convert the full directory into the path, e.g.
  ;; /home/topham/project/Top/myproject --> myproject
  (mapcar (lambda (file) (file-name-nondirectory (directory-file-name file)))
          ;; list all directories in the Top/ folder
          (split-string (shell-command-to-string (format "ls -d %sTop/*" (projectile-project-root))))))

(defun hog-get-project-xml (project)
  "Return the XML (XPR) file for a given Hog PROJECT."
  ;; TODO: add ppr handling
  (format "%sProjects/%s/%s.xpr" (projectile-project-root) project project))

;;;###autoload
(defun hog-open-project (project)
  "Open the Hog PROJECT."
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (let ((command (format "cd %s && source %s && vivado %s &"
                         (projectile-project-root)
                         hog-vivado-path
                         (hog-get-project-xml project)
                         )))
    (message (format "Opening Hog Project %s" project))
    (async-shell-command command)))

(defmacro hog-create-command! (name command)
  "Macro to create a Hog interactive command.
NAME is the function name, COMMAND is the command that should be executed"
  `(defun ,name (project)
     "Project interactive command function"
     (interactive (list (completing-read "Project: "
                                         (hog-get-projects)
                                         nil
                                         t)))
     (hog-run-command ,command project)))

(hog-create-command! hog-create-project "Hog/CreateProject.sh")
(hog-create-command! hog-launch-synthesis (format "Hog/LaunchWorkflow.sh -synth_only -j%d" hog-number-of-jobs))
(hog-create-command! hog-launch-workflow (format "Hog/LaunchWorkflow.sh -j%d" hog-number-of-jobs))
(hog-create-command! hog-launch-impl (format "Hog/LaunchWorkflow.sh -impl_only -j%d" hog-number-of-jobs))

(defun hog-run-command (command project &rest args)
  "Run a Hog COMMAND for a given PROJECT (and colorize it)."

  (let* ((name (format "%s" command))
         (buf (format "*%s*" name)))

    ;; construct the output command
    (let ((cmd-str (format "cd %s && source %s && %s%s %s %s%s | tee hog.log | ccze -A"
                       (projectile-project-root)
                       hog-vivado-path
                       (projectile-project-root)
                       command
                       project
                       (if args " " "")
                       (string-join args " "))))
      ;; ... and run it
      (async-shell-command cmd-str buf))

    ;; change the output buffer to a read-only, evil normal state buffer
    (with-current-buffer buf (evil-normal-state) (view-mode))))

;;--------------------------------------------------------------------------------
;; Intelligence for reading source files...
;;--------------------------------------------------------------------------------

(defun hog-read-lines-from-file (file-path)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun hog-parse-vivado-xml (project-file)
  "Parse a Vivado XPR PROJECT-FILE into a list of libraries and their sources."
  ;; https://stackoverflow.com/questions/43806637/parsing-xml-file-with-elisp
  (require 'xml)
  (setq lib-list (list))
  (dolist (file-node
           ;; get a list of all the Project -> FileSets -> FileSet --> File nodes
           (xml-get-children (assq 'FileSet (assq 'FileSets (assq 'Project (xml-parse-file project-file)))) 'File))
    ;; for each node, extract the path to the .src file
    (setq src-file
          ;; strip off the vivado relative path; make it relative to the repo root instead
          (replace-regexp-in-string "$PPRDIR\/\.\.\/\.\.\/" "" (xml-get-attribute file-node 'Path )))
    ;; for each node, extract the library property (only applies to vhdl sources)
    (dolist (attr (xml-get-children (assq 'FileInfo (cdr file-node)) 'Attr))
      (when (equal (xml-get-attribute attr 'Name) "Library")
        (setq lib  (xml-get-attribute attr 'Val))
        (setf lib-list (hog-append-to-library lib-list lib src-file)))))
  lib-list)

(defun hog-parse-project-xml (project)
  ""
  (hog-parse-vivado-xml (hog-get-project-xml project)))

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

(defun hog-append-to-library (src-list lib-name file-name)
  ""
  (let ((lib (assoc lib-name src-list)))
    (when (eq lib nil)
      (setf src-list (append src-list (list (list lib-name (list)))))
      ;;(print src-list)
      (setq lib (assoc lib-name src-list)))
    (setf (cadr lib) (append (cadr lib) (list file-name) )))
  src-list)

;;------------------------------------------------------------------------------
;; VHDL Tool Config Generation
;;------------------------------------------------------------------------------

(defvar hog-vhdl-tool-preferences
      '(
        ("Preferences" .
         (("TypeCheck"            . "True")
          ("MultiLineErrors"      . "True")
          ("CheckOnChange"        . "True")
          ("Lint"                 . "True")
          ("FirstSyntaxErrorOnly" . "True")))

        ("Lint" .
         (("Threshold" ."Warning")
          ("DeclaredNotAssigned" . (
                                    ("enabled"  . "True")
                                    ("severity" . "Warning")))
          ("DeclaredNotRead"           . "True")
          ("ReadNotAssigned"           . "True")
          ("SensitivityListCheck"      . "True")
          ("ExtraSensitivityListCheck" . "True")
          ("DuplicateSensitivity"      . "True")
          ("LatchCheck"                . "True")
          ("VariableNotRead"           . "True")
          ("PortNotRead"               . "True")
          ("PortNotWritten"            . "True")
          ("NoPrimaryUnit"             . "True")
          ("DuplicateLibraryImport"    . "True")
          ("DuplicatePackageUsage"     . "True")
          ("DeprecatedPackages"        . "True")
          ("ImplicitLibraries"         . "True")
          ("DisconnectedPorts"         . "True")
          ("IntNoRange"                . "True")
          ))))

(defun hog-vhdl-tool-walk-preferences (prefs)
  ""
  (let ((text "") (pad "    "))
    (dolist (category prefs)
      (setq text (concat text (format "%s:\n" (car category))))
      (dolist (pref (cdr category))
        (if (listp (cdr pref))
            (progn
              (setq text (concat text (format "%s%s:\n" pad (car pref))))
              (dolist (subitem (cdr pref))
                (setq text (concat text (format "%s%s%s: %s\n" pad pad (car subitem) (cdr subitem))))))
          (setq text (concat text (format "%s%s: %s\n" pad (car pref) (cdr pref)))))))
    text))

(defun hog-vhdl-tool-lib-to-string (library)
  ""
  (let ((lib-name (car library))
        (lib-files (car (cdr library)))
        (pad "    ")
        (str "")
        )
    (setq str (concat str (format "%s - name: %s\n" pad lib-name)))
    (setq str (concat str (format "%s   paths:\n" pad)))
    (dolist (file lib-files)
      (setq str (concat str (format "%s%s - %s\n" pad pad file))))
    str))

(defun hog-vhdl-tool-parse-libs (libraries)
  ""
  (let ((text "Libraries:\n"))
    (setq libraries (append libraries (list hog-ieee-library)))
    (setq libraries (append libraries (list hog-unisim-library)))
    (dolist (library libraries)
      ;;(concat text (hog-vhdl-tool-lib-to-string library))
      ;;(print (concat text (hog-vhdl-tool-lib-to-string library)))
      (setq text (concat text (hog-vhdl-tool-lib-to-string library)))
      )
    text
    ))

(defun hog-vhdl-tool-create-project-yaml (project)
  ""
  (interactive (list (completing-read "Project: "
                                      (hog-get-projects)
                                      nil
                                      t)))
  (let ((yaml ""))
    (setq yaml (concat yaml (hog-vhdl-tool-parse-libs (hog-parse-project-xml project))))
    (setq yaml (concat yaml (hog-vhdl-tool-walk-preferences hog-vhdl-tool-preferences)))
    (shell-command (format "echo '%s' > %svhdltool-config.yaml" yaml (projectile-project-root)))
    ))

(provide 'hog-emacs)
;;; hog-emacs.el ends here
