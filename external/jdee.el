(when nil
;;JDEE is a package for editing Java code.  It provides intellisense, refactoring tools, and many other features.  It
;;requires CEDET and a JVM installation.  It provides a lot of other features as well, including automatic getter/setter
;;generation, template support, interface stubbing, and a host of other things.

(setq jde-jdk '("1.6"))
(setq jde-jdk-registry (quote (("1.6" . "/usr/lib/jvm/java-6-openjdk/"))))
(require 'cedet)
(require 'jde)

(defun popup-menu (menu &optional position prefix)
  "Popup the given menu and call the selected option.
MENU can be a keymap, an easymenu-style menu or a list of keymaps as for
`x-popup-menu'.
POSITION can be a click event or ((XOFFSET YOFFSET) WINDOW) and defaults to
the current mouse position.
PREFIX is the prefix argument (if any) to pass to the command."
  (let* ((map (cond
               ((keymapp menu) menu)
               ((and (listp menu) (keymapp (car menu))) menu)
               (t (let* ((map (easy-menu-create-menu (car menu) (cdr menu)))
                         (filter (when (symbolp map)
                                   (plist-get (get map 'menu-prop) :filter))))
                    (if filter (funcall filter (symbol-function map)) map)))))
         event cmd)
    (unless position
      (let ((mp (mouse-pixel-position)))
        (setq position (list (list (cadr mp) (cddr mp)) (car mp)))))
    ;; The looping behavior was taken from lmenu's popup-menu-popup
    (while (and map (setq event
                          ;; map could be a prefix key, in which case
                          ;; we need to get its function cell
                          ;; definition.
                          (x-popup-menu position (indirect-function map))))
      ;; Strangely x-popup-menu returns a list.
      ;; mouse-major-mode-menu was using a weird:
      ;; (key-binding (apply 'vector (append '(menu-bar) menu-prefix events)))
      (setq cmd
            (if (and (not (keymapp map)) (listp map))
                ;; We were given a list of keymaps.  Search them all
                ;; in sequence until a first binding is found.
                (let ((mouse-click (apply 'vector event))
                      binding)
                  (while (and map (null binding))
                    (setq binding (lookup-key (car map) mouse-click))
                    (if (numberp binding) ; `too long'
                        (setq binding nil))
                    (setq map (cdr map)))
                  binding)
              ;; We were given a single keymap.
              (lookup-key map (apply 'vector event))))
      ;; Clear out echoing, which perhaps shows a prefix arg.
      (message "")
      ;; Maybe try again but with the submap.
      (setq map (if (keymapp cmd) cmd)))
    ;; If the user did not cancel by refusing to select,
    ;; and if the result is a command, run it.
    (when (and (null map) (commandp cmd))
      (setq prefix-arg prefix)
      ;; `setup-specified-language-environment', for instance,
      ;; expects this to be set from a menu keymap.
      (setq last-command-event (car (last event)))
      ;; mouse-major-mode-menu was using `command-execute' instead.
      (call-interactively cmd))))

;;JDE binds M-d to it's own delete command, which does not work in the case of multiple consecutive capital letters.  We
;;want to rebind this to subword-kill
(define-key jde-mode-map (kbd "M-d") 'subword-kill)
)