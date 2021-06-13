;;; dyncloze-es.el --- Contains commands to start dyncloze for Spanish vocabulary.

;;; Commentary:
;; This file has dyncloze commands specific to Spanish learners. They are meant
;; to be used both as commands you can use, and examples for creating your own
;; commands for your own language practice.

;;; Code:

(defvar dyncloze-es-ser-rx
  (rx (or "ser" "soy" "eres" "es" "somos" "sois" "son" "sido"
          "era" "eras" "era" "éramos" "erais" "eran"
          "fui" "fuiste" "fue" "fuimos" "fuisteis" "fueron"
          "seré" "serás" "será" "seremos" "seréis" "serán"
          "sea" "seas" "seamos" "seáis" "sean"
          "fuera" "fueras" "fuéramos" "fuerais" "fueran"
          "fuese" "fueses" "fuésemos" "feuseis" "feusen"
          "sé" "sed" "sean" "siendo")))

(defvar dyncloze-es-estar-rx
  (rx (or "estar" "estoy" "estás" "está" "estamos" "estáis" "están"
          "estaba" "estabas" "estábamos" "estabais" "estaban"
          "estuve" "estuviste" "etuvo" "estuvimos" "estuvisteis" "estuvieron"
          "estado" "estaré" "estarás" "estará" "estaremos" "estréis" "estarán"
          "estaria" "estarías" "estaría" "estaríamos" "estaríais" "estarían"
          "esté" "estemos" "estad" "estén" "estés" "estéis" "estando")))

(defvar dyncloze-es-imperfect-indicative-rx
  (rx (+ alpha) (or "aba" "abas" "ábamos" "abam" "ía" "ías" "íamos" "íais" "ían")))

(defvar dyncloze-es-preterite-inidicate-rx
  (rx (+ alpha) (or "ei" "aste" "ou" "amos" "aram"
                    "í" "iste" "ió" "imos" "isteis" "ieron")))

(defun dyncloze-es-ser-estar ()
  "Start a dyncloze session for guessing ser/estar and conjucations."
  (interactive)
  (dyncloze (list (cons dyncloze-es-ser-rx "ser")
                  (cons dyncloze-es-estar-rx "estar"))))

(defun dyncloze-es-por-para ()
  "Start a dyncloze session for guessing por vs para."
  (interactive)
  (dyncloze '("por" "para")))

(defun dyncloze-es-preterite-imperfect ()
  "Start a dyncloze session for preterite vs imperfect."
  (interactive)
  (dyncloze (list (cons dyncloze-es-imperfect-indicative-rx "imperfect")
                  (cons dyncloze-es-preterite-inidicate-rx "preterite"))))

(provide 'dyncloze-es)

;;; dyncloze-es.el ends here
