(load (expand-file-name "~/quicklisp/slime-helper.el"))


;; Common Lisp stuff here.
(setq inferior-lisp-program "sbcl")

(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(require 'slime)
(slime-setup)
(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-when-complete-filename-expand t
           slime-truncate-lines nil
           slime-autodoc-use-multiline-p t)
     (slime-setup '(slime-fancy slime-asdf))
     (define-key slime-repl-mode-map (kbd "C-c ;")
       'slime-insert-balanced-comments)
     (define-key slime-repl-mode-map (kbd "C-c M-;")
       'slime-remove-balanced-comments)
     (define-key slime-mode-map (kbd "C-c ;")
       'slime-insert-balanced-comments)
     (define-key slime-mode-map (kbd "C-c M-;")
       'slime-remove-balanced-comments)
     (define-key slime-mode-map (kbd "RET") 'newline-and-indent)
     (define-key slime-mode-map (kbd "C-j") 'newline)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (cond ((not (featurep 'slime))
                   (require 'slime)
                   (normal-mode)))))

(setq prelude-guru nil)

(setq org-directory "~/.org")

(setq org-agenda-files (list "~/.org/anestuary.org"
                             "~/.org/personal.org"))

(setq org-mobile-inbox-for-pull "~/.org/flagged.org")

(setq org-mobile-directory "~/Dropbox/MobileOrg")

(add-to-list 'load-path "/usr/local/bin")

(require 'package)

;(require 'twittering-mode)
;(setq twittering-use-master-password t)


;(require 'org-mobile-sync)
;(org-mobile-sync-mode 1)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(global-prettify-symbols-mode +1)

(package-initialize)

(require 'smartparens-config)

(require 'magit-gh-pulls)

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'subword-mode)

;; Justin's customized org to octopress publishing
;; Based on
;; org-mode 8.x+
;; http://blog.paphus.com/blog/2012/08/01/introducing-octopress-blogging-for-org-mode/
;; https://github.com/yoshinari-nomura/org-octopress
;; See http://wwww.railsonmaui.com

;; the converter from org to jekyll html files
(require 'ox-jekyll)

;; Use plugin for source formatting
(setq org-jekyll-use-src-plugin t)

;; Create YAML front matter in rake task
(setq org-jekyll-include-yaml-front-matter nil)

(defun save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))

(setq org-export-with-sub-superscripts `{})

(setq my-common-octopress-settings
      '(:base-extension "org"
                        :with-toc nil
                        :with-sub-superscript nil
                        :section-numbers nil
                        :recursive t
                        :publishing-function org-jekyll-publish-to-html
                        :headline-levels 4
                        :body-obly t))

(setq my-static-directories '("about" "meta" "tips"))

(setq my-base-directory "~/blag/source")

(defun my-create-octopress-static (prj)
  (let ((base-dir (expand-file-name prj)))
    `(,prj . (:base-directory ,base-dir
                              :publishing-directory ,base-dir
                              ,@my-common-octopress-settings))))

(defun my-static-components ()
  (mapcar 'my-create-octopress-static my-static-directories))

(let ((default-directory my-base-directory))
  (setq org-publish-project-alist
        `(
          ;;components
          ("blog" . (:components ("blog-org" "blog-extra" "about" "meta" "tips")))

          ;;blog articles
          ("blog-org" . (:base-directory ,(expand-file-name "org_posts")
                                         :publishing_directory ,(expand-file-name "_posts")
                                         ,@my-common-octopress-settings))
          ("blog-extra" . (:base-directory ,(expand-file-name "org_posts")
                                           :publishing-directory ,(expand-file-name ".")
                                           :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
                                           :publishing-function org-publish-attachment
                                           :recursive t
                                           :author nil))

          ;;static articles
          ,@(my-static-components))))


(provide 'user)
