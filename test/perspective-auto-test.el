;;; perspective-auto-test.el --- Tests for auto project perspectives -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for modules/interactive/navigation/perspective-auto.el

;;; Code:

(require 'ert)
(require 'project)
(require 'perspective)
(require 'test-helpers)

(require 'navigation/perspective-auto)

(ert-deftest hub/persp-auto-creates-and-switches ()
  "Opening a file in a project creates/switches to its perspective."
  (hub/with-temp-project "src/main.txt"
			 ;; ensure mode is on
			 (hub/persp-auto-project-mode 1)
			 ;; Run deferred switch
			 (hub/test-run-pending-timers 0.1)
			 (let* ((proj (project-current nil))
				(name (funcall hub/persp-name-function proj)))
			   (should (member name (persp-names)))
			   (should (string= (persp-current-name) name))
			   (should (eq (current-buffer) (window-buffer (selected-window)))))))

(ert-deftest hub/persp-auto-switches-between-projects ()
  "Opening a file in a second project switches to that perspective."
  (hub/with-temp-project "foo/a.txt"
			 (hub/persp-auto-project-mode 1)
			 (hub/test-run-pending-timers 0.1)
			 (let* ((proj1 (project-current nil))
				(name1 (funcall hub/persp-name-function proj1)))
			   (should (string= (persp-current-name) name1))
			   ;; second project
			   (hub/with-temp-project "bar/b.txt"
						  (hub/test-run-pending-timers 0.1)
						  (let* ((proj2 (project-current nil))
							 (name2 (funcall hub/persp-name-function proj2)))
						    (should (string= (persp-current-name) name2))
						    (should (not (string= name1 name2))))))))

(provide 'perspective-auto-test)
;;; perspective-auto-test.el ends here
