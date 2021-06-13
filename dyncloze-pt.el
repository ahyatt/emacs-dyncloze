;;; dyncloze-pt.el --- Contains commands to start dyncloze for Portuguese (both Brazilian and Portuguese) vocabulary.

;;; Commentary:
;; This file has dyncloze commands specific to pt-BR and pt-PT. They are meant
;; to be used both as commands you can use, and examples for creating your own
;; commands for your own language practice.

;;; Code:

(defvar dyncloze-pt-ser-rx
  (rx (or "ser" "é" "es" "sou" "somos" "são" "era" "eras" "eramos" "eram"
          "fui" "foi" "foste" "fomos" "foram" "seja" "sejamos" "sejam"
          "fosse" "fora" "foras" "fôssemos" "fossem" "serei" "serás"
          "será" "seramos" "serão" "for" "formos" "forem" "fora"
          "fôramos" "foram" "seria" "serias" "seriámos" "seriam")))

(defvar dyncloze-pt-estar-rx
  (rx (or "estar" "estou" "está" "estás" "estamos" "estão"
          "estava" "estavas" "estavam" "estávamos"
          "estive" "esteve" "estiveste" "estivemos" "estiveram"
          "esteja" "estejamos" "estejam"
          "estivesse" "estiver" "estivera")))

(defvar dyncloze-pt-por-rx
  (rx (or "por" "pela" "pelo")))

(defvar dyncloze-pt-imperfect-indicative-rx
  (rx (+ alpha) (or "ava" "avas" "ávamos" "avam" "ia" "ias" "iam" "íamos")))

(defvar dyncloze-pt-preterite-inidicate-rx
  (rx (+ alpha) (or "ei" "aste" "ou" "amos" "aram"
                    "i" "iste" "iu" "imos" "iram"
                    "este" "eu" "emos" "eram")))

(defun dyncloze-pt-ser-estar ()
  "Start a dyncloze session for guessing ser/estar and conjucations."
  (interactive)
  (dyncloze (list (cons dyncloze-pt-ser-rx "ser")
                  (cons dyncloze-pt-estar-rx "estar"))))

(defun dyncloze-pt-por-para ()
  "Start a dyncloze session for guessing por vs para."
  (interactive)
  (dyncloze (list (cons dyncloze-pt-por-rx "por")
                  "para")))

(defun dyncloze-pt-preterite-imperfect ()
  "Start a dyncloze session for preterite vs imperfect."
  (interactive)
  (dyncloze (list (cons dyncloze-pt-imperfect-indicative-rx "imperfect")
                  (cons dyncloze-pt-preterite-inidicate-rx "preterite"))))

(provide 'dyncloze-pt)

;;; dyncloze-pt.el ends here
