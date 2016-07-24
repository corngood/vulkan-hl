(defun vk-translate-pattern (type wrapper)
  (interactive "sEnter Type:
sEnter Wrapper:")
  (beginning-of-line)
  (let* ((case-fold-search nil)
         (end (line-end-position)))
    (search-forward "pattern " end)
    (search-forward "VK_" end)
    (search-forward type end)
    (search-forward "_" end))
  (let* ((typec (s-upper-camel-case type))
         (enum (buffer-substring (point) (end-of-thing 'symbol)))
         (enumc (s-upper-camel-case enum)))
    (kill-whole-line)
    (insert "pattern " enumc " = " wrapper " VK_" type "_" enum " :: " typec "\n")))

(defun vk-translate-patterns (start end type wrapper)
  (interactive "r
sEnter Type:
sEnter Wrapper:")
  (goto-char start)
  (let* ((input (buffer-substring start end))
         (output
          (with-temp-buffer
            (insert input)
            (goto-char (point-min))
            (while (< (point) (buffer-end 1))
              (delete-blank-lines)
              (vk-translate-pattern type wrapper))
            (buffer-string))))
    (goto-char start)
    (delete-region start end)
    (insert output)))
