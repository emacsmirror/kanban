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
;; #+TBLFM: @1='(kanban-headers $#)::@2$1..@>$>='(kanban-zero $# @#)
;;
;; * Stateful Kanban: Use org-mode to retrieve tasks, but track their state in the Kanban board
;;
;; |   |   |   |
;; |---+---+---|
;; |   |   |   |
;; |   |   |   |
;; #+TBLFM: (kanban-headers $#)::@2$1..@>$>='(kanban @# @2$2..@>$>)


(defun kanban-headers (column)
  "Fill the headers of your table with your org-mode TODO
states. If the table is too narrow, the only the first n TODO
states will be shown, with n as the number of columns in your
table."
  (let ((words org-todo-keywords-1)) (nth (- column 1) words)))

(defun kanban-zero (column row)
  "Zero-state Kanban board: This Kanban board just displays all
org-mode headers which have a TODO state in their respective TODO
state. Useful for getting a simple overview of your tasks."
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

(defun kanban (column cels &optional match)
  "Kanban TODO item grabber. Fills the first row of the kanban
table with org-mode TODO entries, if they are not in another cell
of the table. This allows you to set the state manually and just
use org-mode to supply new TODO entries."
 (let ((elem (nth (- column 1) (delete nil
                                   (org-map-entries
                                    (lambda
                                      ()
                                      (let
                                          ((line (filter-buffer-substring (point) (line-end-position)))
                                           (keyword (nth 0 org-todo-keywords-1)))
                                        (let ((cleanline (nth 1 (split-string line "* "))))
                                          (let ((shortline (substring cleanline 
                                                                      (+ (length keyword) 1) 
                                                                      (min 40 (length cleanline)))))
                                            (let ((clean (if (member " " (split-string 
                                                                          (substring shortline 
                                                                                     (min 25 (length shortline)))
                                                                          ""))
                                                             (mapconcat 'identity 
                                                                        (reverse (rest (reverse 
                                                                                        (split-string shortline " "))))
                                                                        " ") shortline)))
                                              (if (not (member keyword (split-string cleanline " "))) 
                                                  nil
                                                (concat "[[" cleanline "][" clean "]]" )))))))
                                    match 'agenda)))))
   (if
       (or (member elem
               (list cels)) (equal elem nil))
               "" elem)))

(provide 'kanban)