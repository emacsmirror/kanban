;; Use org-tables as kanban tables for more efficient todo tracking.
;;
;; Usage: 
;;
;; * Zero state Kanban: Directly displaying org-mode todo states as kanban board
;;
;; |   |   |   |
;; |---+---+---|
;; |   |   |   |
;; |   |   |   |
;; #+TBLFM: @1='(kanban-zero-headers $#)::@2$1..@>$>='(kanban-zero $# @#)
;;
;; * Stateful Kanban: Use org-mode to retrieve tasks, but track their state in the Kanban board
;;
;; |   |   |   |
;; |---+---+---|
;; |   |   |   |
;; |   |   |   |
;; #+TBLFM: $1='(let ((elem (nth (- @# 1) (delete nil (org-map-entries (lambda () (let ((line (filter-buffer-substring (point) (line-end-position)))(keyword "❢")) (let ((cleanline (nth 1 (split-string line "* ")))) (let ((shortline (substring cleanline (+ (length keyword) 1) (min 40 (length cleanline))))) (let ((clean (if (member " " (split-string (substring shortline (min 25 (length shortline))) "")) (mapconcat 'identity (reverse (rest (reverse (split-string shortline " ")))) " ") shortline))) (if (not (member keyword (split-string cleanline " "))) nil (concat "[[" cleanline "][" clean "]]" ))))))) nil 'agenda))))) (if (member elem (list @2$2..@>$>)) "" elem))::$2='(let ((elem (nth (- @# 1) (delete nil (org-map-entries (lambda () (let ((line (filter-buffer-substring (point) (line-end-position)))(keyword "STARTED")) (let ((cleanline (nth 1 (split-string line "* ")))) (if (not (member keyword (split-string cleanline " "))) nil (concat "[[" cleanline "][" (substring cleanline (+ (length keyword) 1) (min 40 (length cleanline))) "]]" ))))) nil 'agenda))))) (if (member elem (list @2$3..@>$>)) "" (if (equal elem nil) "" elem)))::@1='(let ((words org-todo-keywords-1)) (nth (- $# 1) words))


(defun kanban-zero-headers (column)
  (let ((words org-todo-keywords-1)) (nth (- column 1) words)))

(defun kanban-zero (column row)
  (let
      ((elem (nth (- column 1) 
                  (delete nil (org-map-entries
                               (lambda ()
                                 (let ((line (filter-buffer-substring 
                                              (point) (line-end-position)))
                                       (keyword (nth (- row 1) org-todo-keywords-1)))
                                   (let ((cleanline (nth 1 (split-string line "* "))))
                                     (if (not (member keyword (split-string cleanline " ")))
                                         nil
                                       (concat "[[" cleanline "][" 
                                               (substring cleanline
                                                          (+ (length keyword) 1)
                                                          (min 30 (length cleanline))) "]]" ))))) nil 'agenda)))))
    (if
        (equal
         elem nil) "" elem)))