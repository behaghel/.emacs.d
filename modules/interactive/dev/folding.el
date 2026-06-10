;;; folding.el --- Development folding configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Origami folding and its hydra command surface.

;;; Code:

(use-package origami
  :unless noninteractive
  :commands origami-mode
  :after (hydra)
  :init
  (defhydra hydra-folding (:color red :hint nil)
	    "
 _o_pen node    _n_ext fold       toggle forw_a_rd    _u_ndo            _F_ill column: %`fill-column
 _c_lose node   _p_revious fold   toggle _A_ll        _r_edo            e_x_it
 _z_oom on node
"
	    ("o" origami-open-node)
	    ("c" origami-close-node)
	    ("z" origami-show-only-node)
	    ("u" origami-undo)
	    ("r" origami-redo)
	    ("n" origami-next-fold)
	    ("p" origami-previous-fold)
	    ("a" origami-forward-toggle-node)
	    ("A" origami-toggle-all-nodes)
	    ("F" fill-column)
	    ("x" nil :color blue))
  :bind (:map evil-normal-state-map (",Z" . hydra-folding/body)))

(provide 'dev/folding)
;;; folding.el ends here
