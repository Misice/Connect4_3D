//////////////////////////////////////////////// PRAVI POCETNO STANJE ////////////////////////////////////

(defun Napravi_Matricu (n) 
  (Postavljanje_Pocetnog_Stanja n n)) 


(defun Postavljanje_Pocetnog_Stanja (n m) 
  (cond ((equalp m 0) '() )    
         (t (cons (Postavljanje__Deo_Pocetnog_Stanja (* n n))  (Postavljanje_Pocetnog_Stanja n (- m 1))  )) )) 


(defun Postavljanje__Deo_Pocetnog_Stanja (n) 
  (cond ((equalp n 0) '() )
        (t  (cons '- (Postavljanje__Deo_Pocetnog_Stanja (- n 1) )))))


////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////// KO IGRA PRVI ////////////////////////////////////

(defun koigraprvi ()  (progn (format t "~%Ko igra prvi? (k ili r):") (let ((citaj (read))) (if (equalp citaj 'k) t 
                                                                                              (if (equalp citaj 'r) '() (koigraprvi)  ))  ))) 

////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////// DA LI JE KRAJ  ////////////////////////////////////

(defun Da_Li_Je_Kraj (l) 
  (cond ((null l) '0 )
        ((equal (car l) '-) '1)
       ((atom (car l))  (+ 0 (Da_Li_Je_Kraj (cdr l))) )                        
       ((listp l) (+ (+ 0 (Da_Li_Je_Kraj (car l))) (+ 0 (Da_Li_Je_Kraj (cdr l)))) ) ))



////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////// ODIGRAJ VALIDAN POTEZ ////////////////////////////////////



(defun odigraj_potez (potez x y z n l)  (if (proveri_potez potez x y z n l)  
                                            (let ( (promenjeno (promeni_stanje potez x y z n l) ) )
                                              (progn (prikazi n promenjeno) promenjeno)) '()))

(defun proveri_potez (potez x y z n l) (cond  ((> x n) '() )                               
                               ((> y n) '() ) 
                               ((> z n) '() ) 
                               ((< x 1) '() ) 
                               ((< y 1) '() ) 
                               ((< z 1) '() ) 
                                       (t (if (equalp '- (nth (+ (*  n  (- x 1))  (- y 1)) (nth (- z 1) l) )  )  t '())) ))





(defun promeni_stanje (potez x y z n l) (cond ((null l) '()) 
                                              ((atom (car l)) (if  (and (and (equalp z 1) (equalp x 1)) (equalp y 1)) (cons potez (promeni_stanje potez -1 -1 -1 n (cdr l))) 
                                             (cond  ((> y 0) (cons (car l) (promeni_stanje potez x (- y 1) z n (cdr l))))
                                                   ((and (> x 0) (< y 1) ) (cons (car l) (promeni_stanje potez (- x 1) (- n 1) z n (cdr l)))  )
                                                (t (cons (car l) (promeni_stanje potez -1 -1 -1 n (cdr l))))   )))    
                                    ((listp (car l)) (cons (promeni_stanje potez x y z n (car l)) (promeni_stanje potez x y (- z 1) n (cdr l)))  )))


////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////// IGRA FUNKCIJA//////////////////////////////

(defun igra (stanje n igrac koigra)
  (if (equal koigra t)
  (progn (format t "~%Unesite potez (vrsta kolona dubina): ") 
    (let* ((potez (read))  
           (novostanje (odigraj_potez igrac (car potez) (nth 1 potez) (nth 2 potez) n stanje))) 
     (cond ((null novostanje)  (progn (format t "~%Uneli ste nevalidno stanje") (igra stanje n igrac koigra)))      
          (t (if (equal (Da_Li_Je_Kraj novostanje) 0) (skor novostanje n)
               (igra novostanje n (if (equalp igrac 'X ) 'O 'X) (if (equalp koigra t ) '() t)) )))))
    (progn (sva_nova_stanja stanje n igrac) (igra stanje n igrac (if (equalp koigra t ) '() t))) )) 

////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////// STAMPANJE ////////////////////////////////////

(defun prikazi (dimenzije stanje)
  
 (format t"~%") 
(loop for i from 0 to (- dimenzije  1) 
	
	do(loop for j from 0 to (- dimenzije  1)  
		
		do(
			
			loop for k from 0 to (- dimenzije  1)  do ( write(nth (+ k (* dimenzije i)) (nth j stanje)))
			
			
		)do(format t " ")
	)
	do(format t "~%")
	)		
)

//////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////// NAPRAVI LISTU SVIH MOGUCIH POTEZA ///////////////////////////////

(defun sva_nova_stanja (stanje n potez)
  (progn (format t "~%LISTA SVIH MOGUCIH POTEZA JE: ") 
    ( napravi_sva_moguca_stanja stanje 1 1 1 n potez) ))


(defun napravi_sva_moguca_stanja (stanje x y z n potez)
  (cond ( (and (and (> y n) (< x (+ n 1))) (> z n))  '() )
        ((and (> y n) (< x (+ n 1)) ) (napravi_sva_moguca_stanja stanje (+ x 1) 1 z n potez) )
        ((and (> x n) (> y n)) (napravi_sva_moguca_stanja stanje 1 1 (+ z 1) n potez) )
        (t (append (odigraj_potez potez x y z n stanje) (napravi_sva_moguca_stanja stanje x (+ y 1) z n potez) )   )))
		
///////////////////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////// POKRETANJE IGRE//////////////////////////////////////////////


(defun ConnectFour3D () 
  (let* ((dimenzije (progn (format t "~%Unesite dimenzije table : ") (read))) (igrac (koigraprvi))) 
    (igra (napravi_matricu dimenzije) dimenzije 'x igrac) ))

(ConnectFour3D)


/////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////// PROVERA POBEDNIKA //////////////////////////////////////////////


(defun skor (stanje dimenzije)
(defvar rezX 0)
(defvar rezO 0)
(horizontale dimenzije stanje  )
(proveri_horizont90 0 0 0 stanje dimenzije  )
(vertikale dimenzije stanje)
(dijagonaleZ_poziv dimenzije stanje)
(dijagonaleY_poziv dimenzije stanje 0)
(dijagonaleX_poziv dimenzije stanje 0)
(dijagonale_glavne 0 0 0 1 1 stanje dimenzije 0)
(dijagonale_glavne 0 0 (- dimenzije 1) -1 1 stanje dimenzije 0)
(dijagonale_glavne (* dimenzije (- dimenzije 1) ) 0 (- dimenzije 1) -1 -1 stanje dimenzije 0)
(dijagonale_glavne (* dimenzije (- dimenzije 1) ) 0 0 1 -1 stanje dimenzije 0)
 (format t "~%Crni ima ovoliko poena : ~a " rezX)
 (format t "~%Beli ima ovoliko poena : ~a " rezO)
(setq rezX 0) (setq rezO 0)
(values)
)


(defun dijagonale_glavne (dubina korak poz smer asc stanje dimenzije rez) 
 (if (>= korak dimenzije) (return-from dijagonale_glavne))
(if (and (equalp (nth poz (nth dubina stanje)) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth poz (nth dubina stanje)) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth poz (nth dubina stanje)) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO)(setq rez 0))))
(dijagonale_glavne (+ dubina (* asc dimenzije) 1 ) (+ korak 1) (+ poz smer) smer asc stanje dimenzije rez)
)



(defun dijagonaleX_poziv (dimenzije stanje br)
(if (>= br dimenzije) (return-from dijagonaleX_poziv))
(dijagonaleX br 0 0 1 stanje dimenzije 0)
(dijagonaleX br 0 (- dimenzije 1) -1 stanje dimenzije 0)
(if (> dimenzije 4) (progn (dijagonaleX br 1 1 1 stanje dimenzije 0) (dijagonaleX (+ br dimenzije) 1 0 1 stanje dimenzije 0) 
(dijagonaleX br 1 (- dimenzije 2) -1 stanje dimenzije 0)(dijagonaleX (+ br dimenzije) 1 (- dimenzije 1) -1 stanje dimenzije 0)))
(if (> dimenzije 5)(progn (dijagonaleX br 2 2 1 stanje dimenzije 0) 
                     (dijagonaleX (+ br (* 2 dimenzije)) 2 0 1 stanje dimenzije 0) 
                     (dijagonaleX br 2 (- dimenzije 3) -1 stanje dimenzije 0)
                     (dijagonaleX (+ br (* 2 dimenzije)) 2 (- dimenzije 1) -1 stanje dimenzije 0)) )
(dijagonaleX_poziv dimenzije stanje (+ br 1))
(values)
) 


(defun dijagonaleX (dubina korak poz smer stanje dimenzije rez) 
 (if (>= korak dimenzije )(return-from dijagonaleX))
(if (and (equalp (nth poz (nth dubina stanje)) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth poz (nth dubina stanje)) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth poz (nth dubina stanje)) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( and (incf rezX)(setq rez 0))(if (<= rez -4) (and (incf rezO)(setq rez 0))))
(values)
(dijagonaleX (+ dubina dimenzije) (+ 1 korak) (+ smer poz )smer stanje dimenzije rez))






(defun dijagonaleZ_poziv (dimenzije stanje)
(if (equal stanje '() ) (return-from dijagonaleZ_poziv))
(dijagonaleZ 0 0 (car stanje) dimenzije 0)
(dijagonaleZL 0 0 (car stanje) dimenzije rez)
(values)
)



(defun dijagonaleZ (dubina korak lista dimenzije rez)
(if (= korak dimenzije) (return-from dijagonaleZ))
(if (and (equalp (nth dubina lista) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth dubina lista) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth dubina lista) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO)(setq rez 0))))
(dijagonalez (+ dubina dimenzije 1) (+ korak 1) lista dimenzije rez)
)


(defun dijagonaleZL (dubina korak lista dimenzije rez)  
  (if (= korak dimenzije) (return-from dijagonaleZL))
(if (and (equalp (nth dubina lista) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth dubina lista) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth dubina lista) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO)(setq rez 0))))
(dijagonaleZL (- (+ dubina dimenzije) 1) (+ korak 1) lista dimenzije rez)
) 





(defun dijagonaleY_poziv (dimenzije stanje br)
(if (>= br (* dimenzije dimenzije)) (return-from dijagonaleY_poziv))
 (dijagonaleY br 0 0 1 stanje dimenzije 0)
 (dijagonaleY br 0 (- dimenzije 1) -1 stanje dimenzije 0)
(if (> dimenzije 4) (progn (dijagonaleY br 1 1 1 stanje dimenzije 0) (dijagonaleY (+ br 1) 1 0 1 stanje dimenzije 0) 
                      (dijagonaleY br 1 (- dimenzije 2) -1 stanje dimenzije 0)
                      (dijagonaleY (+ 1 br) 1 (- dimenzije 1) -1 stanje dimenzije 0)))
(if (> dimenzije 5)(progn (dijagonaleY br 2 2 1 stanje dimenzije 0)(dijagonaleY (+ 2 br) 2 0 1 stanje dimenzije 0) 
                     (dijagonaleY 0 2 (- dimenzije 3) -1 stanje dimenzije 0) 
                     (dijagonaleY (+ 2 br) 2 (- dimenzije 1) -1 stanje dimenzije 0)) )
 (dijagonaleY_poziv dimenzije stanje (+ br dimenzije))
(values)
)

(defun dijagonaleY (dubina korak poz smer stanje dimenzije rez)
(if (>= korak dimenzije )(return-from dijagonaleY))
(if (and (equalp (nth poz (nth dubina stanje)) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth poz (nth dubina stanje)) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth poz (nth dubina stanje)) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO)(setq rez 0))))
(dijagonaleY (+ dubina 1) (+ 1 korak) (+ smer poz ) smer stanje dimenzije rez))






(defun horizontale(dimenzije stanje )
(if (equalp stanje '())(return-from horizontale '()))
(setq rez 0)
(proveri_horizont rez 0 (car stanje) dimenzije  )
(horizontale dimenzije (cdr stanje)  )
)


(defun proveri_horizont(rez dubina lista dimenzije  )
(if ( > dubina (-(* dimenzije dimenzije) 1 ))(return-from proveri_horizont))
(if ( = 0 (mod dubina dimenzije)) (setq rez 0))
(if (and (equalp (nth dubina lista) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth dubina lista) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth dubina lista) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO) (setq rez 0))))
(proveri_horizont rez (incf dubina) lista dimenzije  )
)



(defun proveri_horizont90(rez dubina poz stanje dimenzije  )
(if ( = dubina (* dimenzije dimenzije)) (return-from proveri_horizont90))
(if ( = 0 poz) (setq rez 0))
(if (and (equalp (nth (mod dubina (* dimenzije dimenzije)) (nth (mod poz dimenzije) stanje)) 'X)(>= rez 0)) (incf rez)
  (if (and (equalp (nth (mod dubina (* dimenzije dimenzije))(nth (mod poz dimenzije) stanje)) 'O)(<= rez 0)) (decf rez)
    (if (equalp (nth (mod dubina (* dimenzije dimenzije))(nth (mod poz dimenzije) stanje)) 'X) (setq rez 1)(setq rez -1) )))
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO)(setq rez 0))))
(if (= poz (- dimenzije 1))(proveri_horizont90 rez (+ 1 dubina) 0 stanje dimenzije  )
  (proveri_horizont90 rez dubina (+ 1 poz) stanje dimenzije  ))
)


(defun vertikale (dimenzije stanje  )
(if (equalp stanje '())(return-from vertikale '()))
(setq rez 0)
(proveri_stapic rez 0 (car stanje) dimenzije  )
(vertikale dimenzije (cdr stanje)  )
)


(defun proveri_stapic (rez dubina lista dimenzije  )
  (if  ( > dubina ( - (* dimenzije dimenzije) 1 )) (if (< dubina (+(* dimenzije dimenzije) (- dimenzije 1 ))) 
                  (proveri_stapic 0 (+ (mod dubina dimenzije) 1) lista dimenzije  )(return-from proveri_stapic)))
(if (and (equalp (nth dubina lista) 'X)(>= rez 0)) (incf rez) 
(if (and (equalp (nth dubina lista) 'O)(<= rez 0)) (decf rez) 
(if (equalp (nth dubina lista) 'X) (setq rez 1)(setq rez -1) )))                                                                                                                                                                                                                                                                                                          
(if( >= rez 4)( progn (incf rezX)(setq rez 0))(if (<= rez -4) (progn (incf rezO)(setq rez 0))))	
(proveri_stapic rez (+ dubina dimenzije) lista dimenzije  )
)



/////////////////////////////////////////////////////////////////////////////////////////

