;;; À Supprimer ;;;

(defparameter *grille-difficile*
(list (list 0 0 5 8 4 0 0 2 0)
    (list 2 0 0 6 0 0 0 5 0)
    (list 6 8 1 0 0 9 0 0 0)
    (list 4 0 0 0 0 0 1 0 0)
    (list 8 0 2 0 0 0 7 0 4)
    (list 0 0 7 0 0 0 0 0 2)
    (list 0 0 0 9 0 0 3 1 6)
    (list 0 2 0 0 0 5 0 0 9)
    (list 0 3 0 0 8 1 2 0 0)))


(defparameter *grille-diabolique*
(list (list 0 0 0 0 0 5 0 2 4)
   (list 8 0 1 0 0 4 0 0 0)
   (list 9 0 0 0 3 0 5 8 0)
   (list 0 7 0 5 0 0 0 0 0)
   (list 4 0 0 0 0 0 0 0 9)
   (list 0 0 0 0 0 2 0 6 0)
   (list 0 3 5 0 8 0 0 0 2)
   (list 0 0 0 4 0 0 3 0 1)
   (list 1 4 0 3 0 0 0 0 0)))


(defparameter *gd2*
(list (list 0 9 7 2 1 0 0 0 0)
   (list 0 0 0 8 4 0 5 0 0)
   (list 8 0 0 0 0 6 0 0 0)
   (list 0 8 5 0 9 0 0 0 6)
   (list 0 0 4 0 0 0 2 0 0)
   (list 6 0 0 0 8 0 7 3 0)
   (list 0 0 0 4 0 0 0 0 8)
   (list 0 0 1 0 3 8 0 0 0)
   (list 0 0 0 0 7 1 3 5 0)))

;;; VARIABLES GLOBALES ;;;

; Drapeau pour valider si la grille est complete 
(defparameter *grille-est-complete* 0)
; Drapeau pour valider si les regles ont été respecté
(defparameter *regles-respectees* 0)
; 0 est là car delete ne va pas supprimer le premier élément dans la liste
(defparameter *valeurs-possibles* (list 0 1 2 3 4 5 6 7 8 9))
; Modèle pour véfier que l'utilisateur n'écrase pas les chiffres donné au début du jeu.  
(defparameter *liste-grilles* '())  ; liste de grilles au cas où...


;;; MÉTHODES GÉNÉRALES ;;; 
(defun afficher-grille (grille)
  (setq *rang* 1)
  (format t "~%  | A   B   C | D   E   F | G   H   I  ")
  (format t "~%***************************************~%")
  (loop for i below (car (array-dimensions grille)) do
       (format t "~a " *rang*)
       (setq *rang* (1+ *rang*))
       (loop for j below (cadr (array-dimensions grille)) do
	    (if (not (zerop (aref grille i j)))
		(progn (let ((cellule (aref grille i j)))
			 (if (equal (mod j 3) 0)
			     (format t "| ~a " cellule)
			     (format t "  ~a " cellule))))))
       (format t "|")
       (if (equal (mod (1- *rang*) 3) 0)
	   (format t "~%***************************************~%")
	   (format t "~%"))))



; Demander un chiffre de l'utilisateur
(defun demander-chiffre ()
   (defparameter *drapeau-chiffre* 0)  
   (loop do
      (format t "Choisissez un chiffre entre 1 et 9: ")
      (defparameter *chiffre* (char-code (char (read-line) 0)))	
	(cond ((and (< *chiffre* 58) (> *chiffre* 48)) (progn (setf *drapeau-chiffre* 0) (setf *chiffre* (- *chiffre* 48))))  
               ((and (< *chiffre* 0) (> *chiffre* 10)) (progn (setf *drapeau-chiffre* 0) (setf *chiffre* (- *chiffre* 1))))  
	       (t (progn (format t "Ce chiffre n'est pas valable. ") (setf *drapeau-chiffre* 1))))
     until (zerop *drapeau-chiffre*))
    (clear-input))
         

; Demander un colonne de l'utilisateur
(defun demander-colonne ()
   (defparameter *drapeau-col* 0)
   (loop do
      (format t "Choisissez une colonne (A-I): ")
      (defparameter *colonne* (char-code (char (read-line) 0)))
	 (cond ((and (< *colonne* 74) (> *colonne* 64)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 65))))  ; pour les majuscules
               ((and (< *colonne* 106) (> *colonne* 96)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 97))))  ;pour les minuscules
	       (t (progn (format t "Cette colonne n'est pas valable. ") (setf *drapeau-col* 1))))
    until (zerop *drapeau-col*))
    (clear-input))
         
; Demander un rang de l'utilisateur
(defun demander-rang ()
   (defparameter *drapeau-rang* 0)  
   (loop do
      (format t "Choisissez un rang (1-9): ")
      (defparameter *rang* (char-code (char (read-line) 0)))	
	(cond ((and (< *rang* 58) (> *rang* 48)) (progn (setf *drapeau-rang* 0) (setf *rang* (- *rang* 48))))  
               ((and (< *rang* 0) (> *rang* 10)) (progn (setf *drapeau-rang* 0) (setf *rang* (- *rang* 1))))  
	       (t (progn (format t "Ce rang n'est pas valable. ") (setf *drapeau-rang* 1))))
     until (zerop *drapeau-rang*))
    (clear-input))

; Remplir un cas dans la grille
(defun remplir-grille (y x grille chiffre)
   (defparameter *drapeau-placement* 0)
      (verifier-cas-modifiable x y *grille-modele*)
      (if (zerop *drapeau-placement*)
         (setf (aref grille x y) chiffre) 
        (format t "Vous ne pouvez modifier cette cellule. ~%"))) 
      


;;; MÉTHODES DE VÉRIFICATION/VALIDATION ;;;

; Vérifer que la grille respecte toutes les regles du jeu (sans considerer les 0s)
(defun valider-grille (grille)
  (valider-rangs-et-colonnes grille)
  (valider-carre grille))

; Vérifier que aucun cas n'est pas vide
(defun verifier-tous-cas-complets (grille)
   (defparameter *grille-est-complete* 0)
   (loop for x from 0 to 8 do
     (loop for y from 0 to 8 do
       (if (zerop (aref grille x y))
        (setq *grille-est-complete* 1)))))

; Vérifier si cas est modifiable
(defun verifier-cas-modifiable (x y modele) 
  (if (zerop (aref modele x y))
     (setf *drapeau-placement* 0)    ; Modifiable 
     (setf *drapeau-placement* 1)))  ; Non-modifiable

;fonction pour savoir si ,une fois la grille remplie , elle est gagnante ou pas
(defun verifier-solution(grille solution)
   (defparameter *drapeau* 0)
   (loop for y from 0 to 8 do
     (if (/= *drapeau* 1)
      (loop for x from 0 to 8 do
         (if (/= *drapeau* 1)
            (if (/= (aref grille x y) (aref solution x y))
               (setf *drapeau* 1))))))
      (if (/= *drapeau* 1)
        (format t "Félicitations ! Vous avez gagné !~%")
        (format t "Vous n'avez pas la bonne solution! Trouvez votre erreur pour gagner !~%")))



;;; MÉTHODES AUXILIAIRES ;;;

; Vérifier que chaque chiffre 1-9 apparait uniquement une fois dans chaque carré
(defun valider-carre(grille)
  (setq *liste-carre* (list '(0 0) '(0 3) '(0 6) '(3 0) '(3 3) '(3 6) '(6 0) '(6 3) '(6 6)))
  (loop for x in *liste-carre* do 
    (if (= 0 *regles-respectees*)
    (progn (parcourir-carre grille (first x) (second x)))))) 

; Valider qu'il y a aucune repétition dans le carré (sans considerer les 0s)
(defun parcourir-carre (grille x y) 
  (setq *chiffres* '())
  (setq *regles-respectees* 0)
  (loop for a from 0 to 2 do
    (loop for b from 0 to 2 do 
      (if (not (zerop (aref grille (+ x a) (+ y b))))
        (if (null (find (aref grille (+ x a) (+ y b)) *chiffres*)) ; Si le chiffre n'est pas dans la liste
         (setq *chiffres* (append *chiffres* (list (aref grille (+ x a) (+ y b))))) 
         (setq *regles-respectees* 1))))))


; Convertir liste en tableau
(defun convertir-liste-tableau (liste)
	    (make-array (list (length liste)
			      (length (first liste)))
			:initial-contents liste))
; Convertir tableau en liste
(defun convertir-tableau-liste (tableau)
	   (loop for i below (array-dimension tableau 0)
		 collect (loop for j below (array-dimension tableau 1)
			       collect (aref tableau i j))))


; Valider qu'il y a aucune répétition dans les colonnes et les rangs (sans considerer les 0s)
(defun valider-rangs-et-colonnes (grille)
  (defparameter *regles-respectees* 0)
  (setq *liste-chiffres* '())
  (setq *liste-chiffres-2* '())
  (loop for x from 0 to 8 do
    (loop for y from 0 to 8 do
      (if (and (not (zerop (aref grille x y))) (not (zerop (aref grille y x))))
        (progn (format t " " (aref grille x y)) 
        (if (or (find (aref grille x y) *liste-chiffres*) (find (aref grille y x) *liste-chiffres-2*))
           (progn (setf *regles-respectees* 1)(return))
           (progn (setf *liste-chiffres* (append *liste-chiffres* (list (aref grille x y)))) (setf *liste-chiffres-2* (append *liste-chiffres-2* (list (aref grille y x)))))))))
   (if (= *regles-respectees* 1) 
     (return)
     (progn (setf *liste-chiffres-2* '())(setf *liste-chiffres* '()))))
    (if (/= *regles-respectees* 1) grille))   ; si stop boucle = 1, il y a un problème


; fontion appelé à chaque tour pour savoir si la grille est fini ou pas      
(defun verifier-grille-remplie (grille)
   (defparameter *drapeau-rempli* 0)
    (loop for y from 0 to 8 do
     (if (/= *drapeau-rempli* 1)
      (loop for x from 0 to 8 do
         (if (/= *drapeau-rempli* 1)
            (if (zerop (aref grille x y) )
               (setf *drapeau-rempli* 1)))))
	       )
   (if (zerop *drapeau-rempli* )
	(progn(format t "La grille est entièrement remplie !~%") (verifier-solution grille *grille-solution*))
       (format t "La grille n'est pas entièrement remplie !~%")))

  
      

;;; Méthodes aléatoires ;;;

(defun generer-variables-aleatoires (modele)
  (setf *drap-aleat* 0)
  (loop do 
    (defparameter *colonne* (random 9)) 
    (defparameter *rang* (random 9))
    (format t "col: ~a et ran: ~a !!!" *colonne* *rang*)
    (if (zerop (aref modele *colonne* *rang*))
      (setf *drap-aleat* 1)) 
  until (= *drap-aleat* 1)) 
  (defparameter *chiffre* (1+ (random 9)))); Chiffre entre 1 et 9
  



;;; Methodes intelligence-artificielle ;;;

 
;;;;;;;;;;;;;;;;;;;;; CETTE METHODE N'EST PAS BIEN ECRITE ;;;;;;;;;;;;;;;;;;;;;;;
; Selectionner un carré à parcourir
(defun IA-choisir-chiffre(grille x y valeurs)
  (if (and (and (>= x 0) (< x 3)) (and (>= y 0) (< y 3))) (IA-parcourir-carre grille 0 0 valeurs))
  (if (and (and (>= x 0) (< x 3)) (and (> y 2) (< y 6))) (IA-parcourir-carre grille 0 3 valeurs))
  (if (and (and (>= x 0) (< x 3)) (and (> y 5) (< y 9))) (IA-parcourir-carre grille 0 6 valeurs))
  (if (and (and (> x 2) (< x 6)) (and (>= y 0) (< y 3))) (IA-parcourir-carre grille 3 0 valeurs))
  (if (and (and (> x 2) (< x 6)) (and (> y 2) (< y 6))) (IA-parcourir-carre grille 3 3 valeurs))
  (if (and (and (> x 2) (< x 6)) (and (> y 5) (< y 9))) (IA-parcourir-carre grille 3 6 valeurs))
  (if (and (and (> x 5) (< x 9)) (and (>= y 0) (< y 3))) (IA-parcourir-carre grille 6 0 valeurs))
  (if (and (and (> x 5) (< x 9)) (and (> y 2) (< y 6))) (IA-parcourir-carre grille 6 3 valeurs))
  (if (and (and (> x 5) (< x 9)) (and (> y 5) (< y 9))) (IA-parcourir-carre grille 6 6 valeurs))
  valeurs)


; Enlever les valeurs trouvé dans le carré
(defun IA-parcourir-carre (grille x y valeurs) 
  (delete (aref grille x y) valeurs)
  (delete (aref grille (1+ x) y) valeurs)
  (delete (aref grille (+ x 2) y) valeurs)
  (delete (aref grille x (1+ y)) valeurs)
  (delete (aref grille (1+ x) (1+ y)) valeurs)
  (delete (aref grille (+ x 2) (1+ y)) valeurs)
  (delete (aref grille x (+ y 2)) valeurs)
  (delete (aref grille (1+ x) (+  y 2)) valeurs)
  (delete (aref grille (+ x 2) (+ y 2)) valeurs)
  valeurs)

; Enlever les valeurs trouvé dans la colonne
(defun IA-parcourir-colonne (grille x valeurs)
  (loop for y from 0 to 8 do
    (delete (aref grille x y) valeurs)))

; Enlever les valeurs trouvé dans le rang
(defun IA-parcourir-rang (grille y valeurs)
  (loop for x from 0 to 8 do 
    (delete (aref grille x y) valeurs)))

; Retourner une liste de valeurs possible pour un créneau
; Si la taille de la liste = 2 (c'est à dire 0 et un autre chiffre sont les seuls chiffres présent dans la liste)
; on va mettre le chiffre dans le créneau tout de suite. 
(defun IA-determiner-valeurs-possibles(grille x y)
  (setq *temp-vp* (copy-list *valeurs-possibles*))
  (IA-choisir-chiffre grille x y *temp-vp*)
  (IA-parcourir-rang grille y *temp-vp*)
  (IA-parcourir-colonne grille x *temp-vp*)
  (if (= 2 (list-length *temp-vp*))
    (progn (setf (aref grille x y) (second *temp-vp*))(setf *drapeau-solution-complexe* 0))) ; Solution est simple 
  *temp-vp*)

; Retourner une liste de forme ((#ValeursPossibles x y)(#ValeursPossibles2 x2 y2)...) 
; pour chaque créneau pour déterminer quel créneau est à remplir d'abord.
(defun IA-determiner-solutions-possibles (grille)
  (defparameter *liste* '())
  (setq *drapeau-solution-complexe* 1) ; 1 si la solution est complexe, 0 si elle est simple 
  (loop for x from 0 to 8 do 
    (loop for y from 0 to 8 do
      (if (zerop (aref grille x y))
         (setq *liste* (append *liste* (list (list (1- (length (IA-determiner-valeurs-possibles grille x y)))x y)))))))
   (setq *liste* (reverse (sort *liste* #'> :key #'car)))
   *liste*)

; à regarder... 
; retourner une liste de forme ((x y ... solutionsPossibles)(x2 y2 ... solutionsPossibles2)... grille)
(defun backtrack(liste grille)
  (defparameter *solutions-complexes* '())
  (loop for x in liste do
    (if (and (> (first x) 1) (zerop (list-length *solutions-complexes*)))
      (progn (setq *temp* (IA-determiner-valeurs-possibles grille (second x) (third x))) 
        (setq *solutions-complexes* (append *solutions-complexes* (list (second x) (third x))(cdr *temp*))))))*solutions-complexes*)


; Donné une liste de solutions possibles, essayer d'aller plus en profondeur avec chaque solution proposée
(defun IA-tester-possibilities (grille liste) 
  (loop for i in (cdr (cdr liste)) do
    (setq *grille-actuelle* (convertir-liste-tableau (first (last *liste-grilles*))))
    (remplir-grille  (second liste) (first liste) *grille-actuelle* i)
    (IA-aller-plus-en-profondeur *grille-actuelle*)))

; Essayer de remplir grille.  Si la grille n'est plus valable, revenir en arrière. Si elle est valable, continuer d'aller plus en profondeur 
(defun IA-aller-plus-en-profondeur (grille )
  (setq *liste-grilles* (append *liste-grilles* (list (convertir-tableau-liste grille))))
  (setq *solutions* (IA-determiner-solutions-possibles grille))
  (valider-grille grille)
  (if (zerop *regles-respectees*) 
      (progn
        (verifier-tous-cas-complets grille)
        (if (zerop *grille-est-complete*)
          (setq *grille-solution* grille)
          (progn (if (= (first (first *solutions*)) 1)
            (IA-aller-plus-en-profondeur grille))
          (if (>= (first (first *solutions*)) 2)
            (IA-tester-possibilities grille (backtrack *solutions* grille)))))))
  (setq *liste-grilles* (remove (first (last *liste-grilles*)) *liste-grilles* )))


;;; METHODES OBLIGATOIRES ;;;

; Initialiser toutes les variables et determiner la solution de la grille fournie.
(defun init-standalone (grille)
  (defparameter *drapeau-solution-complexe* 1)
  (defparameter *grille-modele* (make-array '(9 9) :initial-contents grille))
  (defparameter *grille-modifiable* (make-array '(9 9) :initial-contents grille))
  (defparameter *grille-solution* (make-array '(9 9) :initial-contents grille))
  (setq *liste-grille* '())
  
  (loop do
    (setq *solutions* (IA-determiner-solutions-possibles *grille-solution*))
    (verifier-tous-cas-complets *grille-solution*)
    (if (and (= *drapeau-solution-complexe* 1)(= 1 *grille-est-complete*))
      (progn (setq *liste-grilles* (append *liste-grilles*  (list (convertir-tableau-liste *grille-solution*)))) 
             (IA-tester-possibilities *grille-solution* (backtrack *solutions* *grille-solution*))))
    (verifier-tous-cas-complets *grille-solution*)
   until (= *grille-est-complete* 0)))

; ...... franchment je comprends toujours pas le but cette méthode...
(defun main-standalone())

   
;;; LE JEU ;;;

(defun sudoku(grille)
   (format t "Welcome ! Bienvenue ! Bienvenido ! Wilkommen ! ~%")
   (defparameter *drapeau* 1)
   (loop do 
      (format t "Choisissez votre mode : ~%")
      (format t "'interactif' pour jouer tout seul, 'aleatoire' pour voir la stratégie aléatoire, ou 'ia' pour résoudre la grille fournie avec une Intelligence Artificielle : ")
      (defparameter *jeu* (read-line))
      (cond 
         ((string-equal *jeu* "interactif") (sudoku-interactive grille))
         ((string-equal *jeu* "aleatoire") (sudoku-aleatoire grille))
         ((string-equal *jeu* "ia") (sudoku-ia grille) )
         (t (format t "Cet option n'existe pas.  Veuillez réessayer. ~%")))
   until (zerop *drapeau*)))

(defun sudoku-interactive(grille-interact)
   (init-standalone grille-interact)
   (loop do 
      (afficher-grille *grille-modifiable*)
      (demander-rang)
      (demander-colonne)
      (demander-chiffre)
      (remplir-grille *colonne* *rang* *grille-modifiable* *chiffre*)
      (verifier-grille-remplie *grille-modifiable*)
    until (zerop *drapeau*)))

(defun sudoku-aleatoire(grille-aleat)
  (init-standalone grille-aleat)
  (afficher-grille *grille-modifiable*)
  (format t "Veuillez patienter -- la version aléatoire peut prendre du temps à compléter. ~%")
  (loop do
     (generer-variables-aleatoires *grille-modele*)
     (remplir-grille *colonne* *rang* *grille-modifiable* *chiffre*)
     (verifier-grille-remplie *grille-modifiable*)
  until (zerop *drapeau*))
  (afficher-grille *grille-modifiable*))


(defun sudoku-ia(grille-ia)
  (init-standalone grille-ia)
  (afficher-grille *grille-solution*)
  (format t "~% Voila ! Notre Intelligence Artificielle a résolu la grille que vous avez fournie. ~%"))



;;; COMMENTAIRES GENERALES

;;;;; À faire....
;; Organisation...
; Variables Globales 
; Méthodes Générales
; Méthodes Aléatoires
; Méthodes Intelligence-Artificielle
; Méthodes obligatoires (init-standalone et l'autre, qui est toujours à implémenter)
; Jeu

;; Style
; Mettre tout les méthodes etc. en français
; Formatter le code àfin de suivre les standards Lisp
; 

;; Extras (?)
; Ajouter un nouveau mode qui suggère à l'utilisateur une liste de chiffre valide pour la case spécifiée (mode: Interactif avec aide d'une IA) 

;; Validation
; Vérifier que l'utilisateur ne peut pas.... 

;; À Completer
; Formatter la grille dans une manière plus jolie 

;; TESTS
; Vérifier que tous les tests passent avec le testeur de la prof
; Tester à la main le mode aléatoire et le mode interactif
; Vérifier combien de temps il faut pour éxécuter le mode IA 
