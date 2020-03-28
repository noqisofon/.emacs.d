(defconst geiser-elisp-dir nil)
(setq geiser-elisp-dir (file-name-directory load-file-name))
(add-to-list 'load-path geiser-elisp-dir)

(require 'geiser)

(setq geiser-scheme-dir "/d/home/rihine/.gasket/repositories/geiser/scheme")

(provide 'geiser-load)
