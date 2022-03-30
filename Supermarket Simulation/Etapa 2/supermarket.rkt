#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 null)
)


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (cond
    ((null? counters) counters)
    ((= (match (car counters) [(counter index tt et queue) index]) index)
     (cons (f (car counters)) (update f (cdr counters) index)))      
    (else (cons (car counters) (update f (cdr counters) index)))
  )
)

; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define (tt+ minutes)
  (lambda (x)
    (struct-copy counter x [
                            tt
                            (match x [(counter index tt et queue) (+ tt minutes)])
                           ]
    )
  )
)


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (et+ minutes)
  (lambda (x)
    (
      (tt+ minutes)
      (
       struct-copy counter x [
                              et
                              (match x [(counter index tt et queue) (+ et minutes) ])
                             ]
      )
    )
  )
)


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define (add-to-counter name n-items)
  (lambda (x)
    (if (null? (match x [(counter index tt et queue) queue]))
        (struct-copy counter x
                     [
                      index
                      (match x [(counter index tt et queue) index])
                     ]
                     [
                      tt
                      (match x [(counter index tt et queue) (+ tt n-items)])
                     ]
                     [
                      et
                      (match x [(counter index tt et queue) (+ et n-items)])
                     ]
                     [
                      queue
                      (match x [(counter index tt et queue) (cons (cons name n-items) queue)])
                     ]
        )
        (struct-copy counter x
                     [
                      index
                      (match x [(counter index tt et queue) index])
                     ]
                     [
                      tt
                      (match x [(counter index tt et queue) (+ tt n-items)])
                     ]
                     [
                      et
                      (match x [(counter index tt et queue) et])
                     ]
                     [
                      queue
                      (match x [(counter index tt et queue)
                                (reverse (cons (cons name n-items) (reverse (match x [(counter index tt et queue) queue]))))])
                     ]
        )
    )
  )
)


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
(define (min-tt-sau-min-et alegere counters)
  (min-tt-sau-min-et-helper alegere counters 0 999999999999)
)

(define (min-tt-sau-min-et-helper alegere counters index tt-min-sau-et-min)
  (cond
    ((null? counters) (cons index tt-min-sau-et-min))
    ((< (match (car counters) [(counter index tt et queue) (if (= alegere 1) tt et)]) tt-min-sau-et-min)
     (min-tt-sau-min-et-helper alegere (cdr counters)
                               (match (car counters) [(counter index tt et queue) index])
                               (match (car counters) [(counter index tt et queue) (if (= alegere 1) tt et)])))
    ((= (match (car counters) [(counter index tt et queue) (if (= alegere 1) tt et)]) tt-min-sau-et-min)
     (min-tt-sau-min-et-helper alegere (cdr counters)
                               (min (match (car counters) [(counter index tt et queue) index]) index)
                               (match (car counters) [(counter index tt et queue) (if (= alegere 1) tt et)])))    
    (else (min-tt-sau-min-et-helper alegere (cdr counters) index tt-min-sau-et-min))
  )
)

(define (min-tt counters) (min-tt-sau-min-et 1 counters)) ; folosind funcția de mai sus
(define (min-et counters) (min-tt-sau-min-et 2 counters)) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (if (null? (match C [(counter index tt et queue) (cdr queue)]))
      (struct-copy counter C
                   [
                    index
                    (match C [(counter index tt et queue) index])
                   ]
                   [
                    tt
                    (match C [(counter index tt et queue) 0])
                   ]
                   [
                    et
                    (match C [(counter index tt et queue) 0])
                   ]
                   [
                    queue
                    (match C [(counter index tt et queue) null])
                   ]
      )
      (struct-copy counter C
                   [
                    index
                    (match C [(counter index tt et queue) index])
                   ]
                   [
                    tt
                    (match C [(counter index tt et queue) (- tt (match C [(counter index tt et queue) et]))])
                   ]
                   [
                    et
                    (match C [(counter index tt et queue) (cdr (cadr queue))])
                   ]
                   [
                    queue
                    (match C [(counter index tt et queue) (cdr queue)])
                   ]
      )
  )
)
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)
(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average)
         (if (
              <=
              (
               /
               (suma-totala-de-tt-uri (append fast-counters slow-counters) 0)
               (+ (numar-case-de-tip-fast fast-counters) (numar-case-de-tip-slow fast-counters slow-counters))
              )
              average
             )
             (
              serve (cdr requests) fast-counters slow-counters
             )
             (
              serve (cdr requests)
                    fast-counters
                    (rezolvare-ensure fast-counters
                                      slow-counters
                                      (numar-case-de-tip-fast fast-counters)
                                      (numar-case-de-tip-slow fast-counters slow-counters)
                                      average
                    )
             )
         )
        ]
        [(list 'remove-first)
         (if (null? (min-et-nou (append fast-counters slow-counters)))
             (serve (cdr requests)
                    fast-counters
                    slow-counters)
             (if (
                  <=
                  (car (min-et (min-et-nou (append fast-counters slow-counters))))
                  (numar-case-de-tip-fast fast-counters)
                 )
                 (serve (cdr requests)
                        (rezolvare-remove
                                          fast-counters
                                          slow-counters
                                          null
                                          1
                                          (car (min-et (min-et-nou (append fast-counters slow-counters))))
                        )
                        slow-counters
                 )
                 (serve (cdr requests)
                        fast-counters
                        (rezolvare-remove
                                          fast-counters
                                          slow-counters
                                          null
                                          2
                                          (car (min-et (min-et-nou (append fast-counters slow-counters))))
                        )
                 )
             )
         )
        ]
        [(list 'delay index minutes)
         (serve (cdr requests)
                (rezolvare-delay fast-counters slow-counters null 1 index minutes)
                (rezolvare-delay fast-counters slow-counters null 2 index minutes))
        ]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (if (<=
                  (car (min-tt (append fast-counters slow-counters)))
                  (numar-case-de-tip-fast fast-counters)
                 )
                 (serve (cdr requests)
                        (rezolvare-add fast-counters slow-counters null 1 name n-items)
                        slow-counters)
                 (serve (cdr requests)
                        fast-counters
                        (rezolvare-add fast-counters slow-counters null 2 name n-items))
              )
             (serve (cdr requests)
                    fast-counters
                    (rezolvare-add fast-counters slow-counters null 2 name n-items))
         )
        ]
      )
  )
)

(define (numar-case-de-tip-fast fast-counters)
  (match (car (reverse fast-counters)) [(counter index tt et queue) index])
)

(define (numar-case-de-tip-slow fast-counters slow-counters)
  (- (match (car (reverse slow-counters)) [(counter index tt et queue) index]) (numar-case-de-tip-fast fast-counters))
)

(define (min-et-nou counters)
  (min-et-nou-helper counters null)
)

(define (min-et-nou-helper counters noua-lista-de-case)
  (if (null? counters)
      noua-lista-de-case
      (if (
           null?
           (match (car counters) [(counter index tt et queue) queue])
          )
          (
           min-et-nou-helper (cdr counters) noua-lista-de-case
          )
          (
           min-et-nou-helper (cdr counters) (append noua-lista-de-case (list (car counters)))
          )
      )
  )
)

(define (suma-totala-de-tt-uri counters suma-tt)
  (if (null? counters)
      suma-tt
      (
       suma-totala-de-tt-uri (cdr counters) (+ suma-tt (match (car counters) [(counter index tt et queue) tt]))
      )
  )
)

(define (rezolvare-ensure fast-counters slow-counters nr-case-fast-counters nr-case-slow-counters average)
  (
   if (
       <=
       (
        /
        (suma-totala-de-tt-uri (append fast-counters slow-counters) 0)
        (+ nr-case-fast-counters nr-case-slow-counters)
       ) 
       average
      )
      slow-counters
      (
       rezolvare-ensure fast-counters
                        (append slow-counters
                                (list
                                      (empty-counter (
                                                      +
                                                      (+ nr-case-fast-counters nr-case-slow-counters)
                                                      1
                                                     )
                                      )
                                )
                        )
                        nr-case-fast-counters
                        (+ nr-case-slow-counters 1)
                        average
      )  
  )
)

(define (rezolvare-remove fast-counters slow-counters lista-auxiliara-remove optiune index)
  (if (null? (if (= optiune 1) fast-counters slow-counters))
      lista-auxiliara-remove
      (if (
           =
           (match (car (if (= optiune 1) fast-counters slow-counters)) [(counter index tt et queue) index])
           index
          )
          (
           append
           (append lista-auxiliara-remove (list (remove-first-from-counter (car (if (= optiune 1) fast-counters slow-counters)))))
           (cdr (if (= optiune 1) fast-counters slow-counters))
          )
          (
           rezolvare-remove (if (= optiune 1) (cdr fast-counters) fast-counters)
                            (if (= optiune 1) slow-counters (cdr slow-counters))
                            (append lista-auxiliara-remove (list (car (if (= optiune 1) fast-counters slow-counters))))
                            optiune
                            index
          )
      )
  )
)

(define (rezolvare-delay fast-counters slow-counters lista-auxiliara-delay optiune index minutes)
  (if (null? (if (= optiune 1) fast-counters slow-counters))
      lista-auxiliara-delay
      (if (= (match (car (if (= optiune 1) fast-counters slow-counters)) [(counter index tt et queue) index]) index)
          (
           append
           (append lista-auxiliara-delay (list ((et+ minutes) (car (if (= optiune 1) fast-counters slow-counters)))))
           (cdr (if (= optiune 1) fast-counters slow-counters))
          )
          (
           rezolvare-delay (if (= optiune 1) (cdr fast-counters) fast-counters)
                           (if (= optiune 1) slow-counters (cdr slow-counters))
                           (append lista-auxiliara-delay (list (car (if (= optiune 1) fast-counters slow-counters))))
                           optiune
                           index
                           minutes
          )
      )
  )
)

(define (rezolvare-add fast-counters slow-counters lista-auxiliara-add optiune name n-items)
  (if (null? (if (= optiune 1) fast-counters slow-counters))
      lista-auxiliara-add
      (if (
           =
           (match (car (if (= optiune 1) fast-counters slow-counters)) [(counter index tt et queue) index])
           (car (min-tt (if (= optiune 1) fast-counters slow-counters)))
          )
          (
           append
           (append lista-auxiliara-add (list ((add-to-counter name n-items) (car (if (= optiune 1) fast-counters slow-counters)))))
           (cdr (if (= optiune 1) fast-counters slow-counters))
          )
          (
           rezolvare-add (if (= optiune 1) (cdr fast-counters) fast-counters)
                         (if (= optiune 1) slow-counters (cdr slow-counters))
                         (append lista-auxiliara-add (list (car (if (= optiune 1) fast-counters slow-counters))))
                         optiune
                         name
                         n-items
          )
      )
  )
)

