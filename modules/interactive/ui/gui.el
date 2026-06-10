;;; gui.el --- UI: GUI-specific extras -*- lexical-binding: t; -*-

;;; Commentary:
;; GUI-only visual enhancements layered on top of ui/core.

;;; Code:

(require 'hub-utils)

(require 'ui/performance)

(defvar hub/performance--gui-load-start (current-time)
  "Time when GUI configuration started loading.")

(hub/performance-log-startup-event "ui/gui load start")

(require 'ui/theme)
(require 'ui/fonts)
(require 'ui/dashboard)

(hub/performance-log-startup-event "ui/gui load complete" hub/performance--gui-load-start)

(provide 'ui/gui)
;;; gui.el ends here
