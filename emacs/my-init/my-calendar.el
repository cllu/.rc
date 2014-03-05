;; set up my calendar and holiday.

;; include holidays into org mode
;(setq org-agenda-include-diary t)

(setq calendar-week-start-day 1)
;; show holiday in calendar
(setq mark-holidays-in-calendar t)

;; set all default holidays to nil
(setq holiday-general-holidays nil
      holiday-local-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil
      ;; this one is strange
      calendar-holidays nil
      )

(setq calendar-holidays '(;; some chinese holidays
			  (holiday-chinese 1 1  "春节")
			  (holiday-chinese 1 15 "元宵节")
			  (holiday-chinese 5 5  "端午节")
			  (holiday-chinese 9 9  "重阳节")
			  (holiday-chinese 8 15 "中秋节")
			  ;; some western holidays
			  (holiday-fixed 1 1   "元旦")
			  (holiday-float 5 0 2 "母亲节")
			  (holiday-float 6 0 3 "父亲节")
			  ))


;; set up cal-china-plus. provide for orgmode chinese calendar.
(require 'cal-china-plus)
(add-hook 'diary-nongregorian-listing-hook 'diary-chinese-list-entries)
(add-hook 'diary-nongregorian-marking-hook 'diary-chinese-mark-entries)

(provide 'my-calendar)
