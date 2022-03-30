#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 null)
)
; Explicatii: aici doar am initializat o structura


; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (struct-copy counter C [
                          tt
                          (match C [(counter index tt queue) (+ tt minutes)])
                          ]
  )
)
; Explicatii: aici am extras din structura tt-ul, l-am adunat cu valoarea lui minutes
; dupa care am realizat o noua structura in care am pastrat totul la fel in afara de
; tt pe care l-am inlocuit cu noul tt


; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (min-tt counters)
  (min-tt-helper counters 0 999999999999)
)

(define (min-tt-helper counters index ttmin)
  (cond
    ((null? counters) (cons index ttmin))
    ((< (match (car counters) [(counter index tt queue) tt]) ttmin)
     (min-tt-helper (cdr counters)
                    (match (car counters) [(counter index tt queue) index])
                    (match (car counters) [(counter index tt queue) tt])))
    ((= (match (car counters) [(counter index tt queue) tt]) ttmin)
     (min-tt-helper (cdr counters)
                    (min (match (car counters) [(counter index tt queue) index]) index)
                    (match (car counters) [(counter index tt queue) tt])))    
    (else (min-tt-helper (cdr counters) index ttmin))
  )
)
; Explicatii: aici am realizat o functie de ajutor, in care am ca argumente lista de
; case, indexul unei case si tt-ul acesteia. (match (car counters) [(counter index tt queue) tt])
; este tt-ul primei case, pe care il compar cu un tt initializat cu o valoare mare de catre mine,
; urmand ulterior sa apelez functia de ajutor in functie de acest rezultat


; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (struct-copy counter C
               [
                index
                (match C [(counter index tt queue) index])
               ]
               [
                tt
                (match C [(counter index tt queue) (+ tt n-items)])
               ]
               [
                queue
                (match C [
                          (counter index tt queue)
                          (reverse (cons (cons name n-items) (reverse (match C [(counter index tt queue) queue]))))]
                )
               ]
  )
)
; Explicatii: aici am realizat o noua structura, unde am pastrat indexul lui C,
; am marit cu n-items valoare lui tt, deoarece am pus o noua persoana la casa,
; astfel timpul de asteptare la casa respectiva a crescut cu un timp egal cu
; numarul de articole pe care le avea persoana respectiva, iar la final
; am introdus perechea nou formata la sfarsitul cozii de asteptare,
; aplicand functia reverse de doua ori pe lista ce reprezinta coada
; cu persoanele care asteapta


; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)
         (cond
           ((= index 1) (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4))
           
           ((= index 2) (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4))
           
           ((= index 3) (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4))
           
           (else (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes)))
         )
        ]
        [(list name n-items)
         (cond
           ((and (<= n-items ITEMS) (= (min-tt-nou (list C1 C2 C3 C4)) 1))
            (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4))
           
           ((and (<= n-items ITEMS) (= (min-tt-nou (list C1 C2 C3 C4)) 2))
            (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))
           
           ((and (<= n-items ITEMS) (= (min-tt-nou (list C1 C2 C3 C4)) 3))
            (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))
           
           ((and (<= n-items ITEMS) (= (min-tt-nou (list C1 C2 C3 C4)) 4))
            (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items)))
           
           ((and (> n-items ITEMS) (= (min-tt-nou (list C2 C3 C4)) 2))
            (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))
           
           ((and (> n-items ITEMS) (= (min-tt-nou (list C2 C3 C4)) 3))
            (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))
           
           (else (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items)))
         )
        ]
      )
  )
)

(define (min-tt-nou counters)
  (min-tt-helper-nou counters 0 999999999999)
)

(define (min-tt-helper-nou counters index ttmin)
  (cond
    ((null? counters) index)
    ((< (match (car counters) [(counter index tt queue) tt]) ttmin)
     (min-tt-helper-nou (cdr counters)
                    (match (car counters) [(counter index tt queue) index])
                    (match (car counters) [(counter index tt queue) tt])))
    ((= (match (car counters) [(counter index tt queue) tt]) ttmin)
     (min-tt-helper-nou (cdr counters)
                    (min (match (car counters) [(counter index tt queue) index]) index)
                    (match (car counters) [(counter index tt queue) tt])))    
    (else (min-tt-helper-nou (cdr counters) index ttmin))
  )
)
; Explicatii: aici am facut mai intai cazul listei cu intarzieri, dupa care am facut
; cazul listei cu persoane ce trebuie asezate la coada la o casa. Aici am realizat o functie
; separata denumita min-tt-nou cu rol in calcularea indexului casei cu tt-ul cel mai mic.
; Apoi am facut multiple comparatii pentru a determina exact casa la care trebuie sa mearga
; respectiva persoana
