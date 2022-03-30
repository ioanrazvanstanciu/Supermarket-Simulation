#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index) ; testată de checker
  (make-counter index 0 0 empty-queue)
)


(define (update f counters index)
  (cond
    ((null? counters) counters)
    ((= (match (car counters) [(counter index tt et queue) index]) index)
     (cons (f (car counters)) (update f (cdr counters) index)))      
    (else (cons (car counters) (update f (cdr counters) index)))
  )
)


(define (tt+ minutes)
  (lambda (x)
    (struct-copy counter x [
                            tt
                            (match x [(counter index tt et queue) (+ tt minutes)])
                           ]
    )
  )
)


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


(define (add-to-counter name n-items) ; testată de checker
  (λ (C) ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (match C [(counter index tt et queue) queue]))
        (struct-copy counter C
                     [
                      index
                      (match C [(counter index tt et queue) index])
                     ]
                     [
                      tt
                      (match C [(counter index tt et queue) (+ tt n-items)])
                     ]
                     [
                      et
                      (match C [(counter index tt et queue) (+ et n-items)])
                     ]
                     [
                      queue
                      (enqueue (cons name n-items) (match C [(counter index tt et queue) queue]))
                     ]
        )
        (struct-copy counter C
                     [
                      index
                      (match C [(counter index tt et queue) index])
                     ]
                     [
                      tt
                      (match C [(counter index tt et queue) (+ tt n-items)])
                     ]
                     [
                      et
                      (match C [(counter index tt et queue) et])
                     ]
                     [
                      queue
                      (enqueue (cons name n-items) (match C [(counter index tt et queue) queue]))
                     ]
        )
    )
  )
)


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


(define (min-tt counters) (min-tt-sau-min-et 1 counters))
(define (min-et counters) (min-tt-sau-min-et 2 counters))


(define (remove-first-from-counter C) ; testată de checker
  (if (
       or
       (= (queue-size-l (match C [(counter index tt et queue) queue])) 1)
       (= (queue-size-r (match C [(counter index tt et queue) queue])) 1)
      )
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
                    empty-queue
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
                    (match C [(counter index tt et queue) (cdr (top (dequeue queue)))])
                   ]
                   [
                    queue
                    (match C [(counter index tt et queue) (dequeue queue)])
                   ]
      )
  )
)


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C
                 [
                  index
                  (match C [(counter index tt et queue) index])
                 ]
                 [
                  tt
                  (match C [(counter index tt et queue)
                            (
                             if (> minutes tt)
                                0
                                (- tt minutes)
                            )
                           ]
                  )
                 ]
                 [
                  et
                  (match C [(counter index tt et queue)
                            (
                             if (> minutes et)
                                0
                                (- et minutes)
                            )
                           ]
                  )
                 ]
                 [
                  queue
                  (match C [(counter index tt et queue) queue])
                 ]
    )
  )
)
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters null)
)

(define (serve-helper requests fast-counters slow-counters lista-cu-iesiri)
  (if (null? requests)
      (cons lista-cu-iesiri (append fast-counters slow-counters))
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
              serve-helper (cdr requests) fast-counters slow-counters lista-cu-iesiri
             )
             (
              serve-helper (cdr requests)
                           fast-counters
                           (
                            rezolvare-ensure fast-counters
                                             slow-counters
                                             (numar-case-de-tip-fast fast-counters)
                                             (numar-case-de-tip-slow fast-counters slow-counters)
                                             average
                           )
                           lista-cu-iesiri
             )
         )
        ]
        [(list 'delay index minutes)
         (
          serve-helper (cdr requests)
                       (rezolvare-delay fast-counters slow-counters null 1 index minutes)
                       (rezolvare-delay fast-counters slow-counters null 2 index minutes)
                       lista-cu-iesiri
         )
        ]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (
              if (
                  <=
                  (car (min-tt (append fast-counters slow-counters)))
                  (numar-case-de-tip-fast fast-counters)
                 )
                 (serve-helper (cdr requests)
                               (rezolvare-add fast-counters slow-counters null 1 name n-items)
                               slow-counters
                               lista-cu-iesiri)
                 (serve-helper (cdr requests)
                               fast-counters
                               (rezolvare-add fast-counters slow-counters null 2 name n-items)
                               lista-cu-iesiri)
             )
             (
              serve-helper (cdr requests)
                           fast-counters
                           (rezolvare-add fast-counters slow-counters null 2 name n-items)
                           lista-cu-iesiri
             )
         )
        ]
        [minutes
         (serve-helper (cdr requests)
                       (reverse (rezolvare-iesire-case fast-counters slow-counters 1 minutes))
                       (reverse (rezolvare-iesire-case fast-counters slow-counters 2 minutes))
                       (
                        append
                        lista-cu-iesiri
                        (
                         append
                         (
                          ordine-buna-lista-cu-iesiri
                                                     (
                                                      sort
                                                      (
                                                       sort
                                                       (
                                                        rezolvare-iesire-lista-cu-iesiri fast-counters minutes
                                                       )
                                                       ordine-buna-indecsi
                                                      )
                                                      ordine-buna-minute
                                                     )
                                                     null
                         )
                         (
                          ordine-buna-lista-cu-iesiri
                                                     (
                                                      sort
                                                      (
                                                       sort
                                                       (
                                                        rezolvare-iesire-lista-cu-iesiri slow-counters minutes
                                                       )
                                                       ordine-buna-indecsi
                                                      )
                                                      ordine-buna-minute
                                                     )
                                                     null
                         )
                        )
                       )
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


(define (suma-totala-de-tt-uri counters suma-tt)
  (if (null? counters)
      suma-tt
      (
       suma-totala-de-tt-uri (cdr counters) (+ suma-tt (match (car counters) [(counter index tt et queue) tt]))
      )
  )
)


(define (ordine-buna-indecsi x y)
  (< (cadr x) (cadr y))  
)


(define (ordine-buna-minute x y)
  (< (cddr x) (cddr y))
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


(define (rezolvare-iesire-case fast-counters slow-counters optiune minutes)
  (foldl
        (
         lambda (x acumulator)
                (rezolvare-iesire-case-helper x acumulator minutes)
        )
        null
        (if (= optiune 1) fast-counters slow-counters)
  )
)


(define (rezolvare-iesire-case-helper C acumulator minutes)
  (
   if (= minutes 0)
      (cons C acumulator)
      (
       if (<= (- (counter-et C) 1) 0)
          (
           if (queue-empty? (counter-queue C))
              (
               rezolvare-iesire-case-helper ((pass-time-through-counter 1) C) acumulator (- minutes 1)
              )
              (
               rezolvare-iesire-case-helper (remove-first-from-counter C) acumulator (- minutes 1)
              )
          )
          (
           rezolvare-iesire-case-helper ((pass-time-through-counter 1) C) acumulator (- minutes 1)
          )
      )
  )                       
)


(define (ordine-buna-lista-cu-iesiri lista-cereri-initiala lista-cereri-finala)
  (
   if (null? lista-cereri-initiala)
      lista-cereri-finala
      (
       ordine-buna-lista-cu-iesiri
                                  (cdr lista-cereri-initiala)
                                  (
                                   append
                                   lista-cereri-finala
                                   (list (cons (cadr (car lista-cereri-initiala)) (car (car lista-cereri-initiala))))
                                  )
      )
  )
)


(define (rezolvare-iesire-lista-cu-iesiri counters minutes)
  (foldl
        (
         lambda (x acumulator)
                (rezolvare-iesire-lista-cu-iesiri-helper x acumulator minutes null minutes)
        )
        null
        counters
  )
)


(define (rezolvare-iesire-lista-cu-iesiri-helper C acumulator minutes lista-cu-iesiri minutele-initiale-nemodificate)
  (
   if (= minutes 0)
      (
       if (null? lista-cu-iesiri)
          acumulator
          (
           append
           acumulator
           lista-cu-iesiri
          )
      )
      (
       if (
           <=
           (- (counter-et C) 1)
           0
          )
          (
           if (queue-empty? (counter-queue C))
              (
               rezolvare-iesire-lista-cu-iesiri-helper
                                                      ((pass-time-through-counter 1) C)
                                                      acumulator
                                                      (- minutes 1)
                                                      lista-cu-iesiri
                                                      minutele-initiale-nemodificate
              )
              (
               rezolvare-iesire-lista-cu-iesiri-helper
                                                      (remove-first-from-counter C)
                                                      acumulator
                                                      (- minutes 1)
                                                      (
                                                       append
                                                       lista-cu-iesiri
                                                       (
                                                        list
                                                            (
                                                             cons
                                                                 (car (top (counter-queue C)))
                                                                 (
                                                                  cons
                                                                      (counter-index C)
                                                                      (- minutele-initiale-nemodificate (- minutes 1))
                                                                 )
                                                            )
                                                       )
                                                      )
                                                      minutele-initiale-nemodificate
              )
          )
          (
           rezolvare-iesire-lista-cu-iesiri-helper
                                                  ((pass-time-through-counter 1) C)
                                                  acumulator
                                                  (- minutes 1)
                                                  lista-cu-iesiri
                                                  minutele-initiale-nemodificate
          )
      )
  )
)

