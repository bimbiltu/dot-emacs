(setq force-load-messages nil)
;; some distros use very high values here
;; doom for example uses most-positive-fixnum
(setq gc-cons-threshold (* 300 1024 1024)
      gc-cons-percentage 0.6)
