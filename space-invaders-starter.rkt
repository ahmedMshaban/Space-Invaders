;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  ListOfInvader ListOfMissile Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1.5))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1.5))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1.5)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))



;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of Invaders

(define LOI1 empty)
(define LOI2 (cons (make-invader 150 100 12)
                   (cons (make-invader 150 HEIGHT -10)
                         (cons (make-invader 150 (+ HEIGHT 10) 10) empty))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self reference: (rest loi) is ListOfInvader



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of Missiles

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300)
                   (cons (make-missile (invader-x I1) (+ (invader-y I1) 10))
                         (cons (make-missile (invader-x I1) (+ (invader-y I1)  5)) empty))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self reference: (rest lom) is ListOfMissile



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main s)
  (big-bang s                        ; Game
    (on-tick   advance-game)         ; Game -> Game
    (to-draw    render-game)         ; Game -> Image
    (stop-when    stop-game)         ; Game -> Boolean
    (on-key      handle-key)))       ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game
;; Game is (make-game  ListOfInvader ListOfMissile Tank)
(check-expect (advance-game
               (make-game empty empty T0)) 
              (make-game  (list-of-invader LOI1) empty (advance-tank T0)))
(check-expect (advance-game
               (make-game LOI2 LOM2 T1))
              (make-game (list-of-invader LOI2) LOM2 ( advance-tank T1)))

;(define (advance-game s) s) ;stub

;; Took template from Game

(define (advance-game s)
  (make-game (list-of-invader (game-invaders s))
             (game-missiles s)
             (advance-tank (game-tank s))))


;; ListOfInvader -> ListOfInvader
;; Produce a list of invadres with the correct x,y and dx values
(check-expect (list-of-invader empty) empty)
(check-expect (list-of-invader
               (cons (make-invader 150 100 1.5)
                   (cons (make-invader 150 HEIGHT -1.5)
                         (cons (make-invader 150 (+ HEIGHT 10) 1.5) empty))))
              (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1.5)
                    (cons (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -1.5)
                          (cons (make-invader (+ 150 INVADER-X-SPEED) (+ (+ HEIGHT 10) INVADER-Y-SPEED) 1.5) empty))))


;(define (list-of-invader loi) empty) ;stub

;; Took template from ListOfInvader

(define (list-of-invader loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
              (list-of-invader (rest loi)))]))


;; Invader -> Invader
;; Produce the next invader x,y,dx positions on the screen
(check-expect (advance-invader (make-invader 150 100 1.5))
              (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1.5))
(check-expect (advance-invader (make-invader 150 HEIGHT -1.5))
              (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -1.5))
(check-expect (advance-invader (make-invader WIDTH 100 1.5))
              (make-invader (- WIDTH (/ (image-width INVADER) 2)) (+ 100 INVADER-Y-SPEED) -1.5))
(check-expect (advance-invader (make-invader 0 100 -1.5))
              (make-invader (/ (image-width INVADER) 2) (+ 100 INVADER-Y-SPEED) 1.5))

;(define (advance-invader invader) (make-invader 150 100 INVADER-X-SPEED)) ;stub

;; Took template from Invader

(define (advance-invader invader)
  (cond [(and
          (>= (invader-x invader) WIDTH)
          (positive? (invader-dx invader)))
         (make-invader (- WIDTH (/ (image-width INVADER) 2)) (+ (invader-y invader) INVADER-Y-SPEED) (- INVADER-X-SPEED))]
        [(and
          (<= (invader-x invader) 0)
          (negative? (invader-dx invader)))
         (make-invader (/ (image-width INVADER) 2) (+ (invader-y invader) INVADER-Y-SPEED) INVADER-X-SPEED)]
        [(negative? (invader-dx invader)) (make-invader (- (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader)) ]
        [else (make-invader (+ (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))]))

;; ListOfMissile -> ListOfMissile
;; Produce the next missile x,y positions on the screen
;; !!!
(define (advance-missile lom) lom) ;stub

;; Tank -> Tank
;; Produce the next tank x position and it's dir on the screen
(check-expect (advance-tank (make-tank (/ WIDTH 2) 1))
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (advance-tank (make-tank 50 1))
              (make-tank (+ 50 TANK-SPEED) 1))          
(check-expect (advance-tank (make-tank (+ WIDTH 2) 1))
              (make-tank (- WIDTH (/ (image-width TANK) 2)) -1))
(check-expect (advance-tank (make-tank 50 -1))
              (make-tank (- 50 TANK-SPEED) -1))         
(check-expect (advance-tank (make-tank (- 0 2) -1))
              (make-tank (/ (image-width TANK) 2) 1))

;(define (advance-tank t) (make-tank (/ WIDTH 2) 1)) ;stub

;; Took template from tank

(define (advance-tank t)
  (cond [(and
          (> (tank-x  t) (- WIDTH (image-width TANK)))
          (= (tank-dir t) 1))
         (make-tank (- WIDTH (/ (image-width TANK) 2)) (-(tank-dir t)))]
        [(and 
          (< (tank-x  t) (image-width TANK))
          (= (tank-dir t) -1))
         (make-tank (/ (image-width TANK) 2) (* (tank-dir t) -1))]
        [(= (tank-dir t)  1) (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]
        [(= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]))


;; Game -> Image
;; render the game
(check-expect (render-game (make-game empty empty T0))
              (place-images
               (list
                (render-tank-img T0)
                (render-inavder-image empty)
                (render-missile-image empty))
               (list 
                (render-tank-pos T0)
                (render-inavder-pos empty)
                (render-missile-pos empty))
               BACKGROUND))
(check-expect (render-game (make-game LOI2 LOM2 T1))
              (place-images
               (list
                (render-tank-img T1)
                (render-inavder-image LOI2)
                (render-missile-image LOM2))
               (list 
                (render-tank-pos T1)
                (render-inavder-pos LOI2)
                (render-missile-pos LOM2))
               BACKGROUND))
       
;(define (render-game s) BACKGROUND) ;stub

;; Took template from Game

(define (render-game s)
  (place-images
   (list
    (render-tank-img (game-tank s))
    (render-inavder-image (game-invaders s))
    (render-missile-image (game-missiles s)))
   (list 
    (render-tank-pos (game-tank s))
    (render-inavder-pos (game-invaders s))
    (render-missile-pos (game-missiles s)))
   BACKGROUND))


;; Tank -> Image
;; Produce the tank image
(check-expect (render-tank-img T0) TANK)
(check-expect (render-tank-img T1) TANK)

;(define (render-tank-img t) TANK) ;stub

;; Took template from Tank

(define (render-tank-img t) TANK)



;; Tank -> Posn
;; render the tank image at the correct location in the screen coordinates
(check-expect (render-tank-pos T0) (make-posn (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)))
(check-expect (render-tank-pos T1) (make-posn 50 (- HEIGHT TANK-HEIGHT/2)))

;(define (render-tank-pos t) (make-posn 0 0)) ;stub

;; Took Template from Tank

(define (render-tank-pos t)
  (make-posn (tank-x t) (- HEIGHT TANK-HEIGHT/2)))



;; ListOfInvader -> Image
;; Produce the inavder image
;; !!!
(define (render-inavder-image loi) empty-image) ;stub


;; ListOfInvader -> ListOfInvader
;; Produce the inavders positions
;; !!!
(define (render-inavder-pos loi) (make-posn 0 0)) ;stub

;; ListOfMissile -> Image
;; Produce the missile image
;; !!!
(define (render-missile-image lom) empty-image) ;stub


;; ListOfMissile -> ListOfMissile
;; Produce the missiles positions
;; !!!
(define (render-missile-pos loi) (make-posn 0 0)) ;stub


;; Game -> Boolean
;; When Invader reaches the bottom of the screen, the game is over. 
;; !!!
(define (stop-game s) false)



;; Game KeyEvent -> Game
;; Hanlde the left key, right key, and space bar key:
;;           - left: move the tank to left at a constant speed
;;           - right: move the tank to right at a constant speed
;;           - space bar: fire missiles straight up from the tank current position
(check-expect (handle-key (make-game empty empty T0) "left")
              (make-game empty empty (tank-diraction T0 "left")))
(check-expect (handle-key (make-game empty empty T0) "right")
              (make-game empty empty (tank-diraction T0 "right")))
(check-expect (handle-key (make-game empty empty T0) "up")
              (make-game empty empty T0))
             
;(define (handle-key s ke) s) ;stub

(define (handle-key s ke)
  (cond [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (tank-diraction (game-tank s) "left"))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (tank-diraction (game-tank s) "right"))]
        [else  s]))


;; Tank String -> Tank
;; Produce the correct tank dirction based on the key click 
(check-expect (tank-diraction (make-tank (/ WIDTH 2) 1) "right")
              (make-tank (/ WIDTH 2) 1))
(check-expect (tank-diraction (make-tank 50 1) "left")
              (make-tank 50 -1))         
(check-expect (tank-diraction (make-tank 50 -1) "right")
              (make-tank 50 1))      

;(define (tank-diraction t ke) (make-tank (/ WIDTH 2) 1)) ;stub

;; Took template from tank

(define (tank-diraction t ke)
  (cond [(and
          (string=? ke "left")
          (= (tank-dir t) 1)) (make-tank (tank-x t) -1)]
        [(and
          (string=? ke "right")
          (= (tank-dir t) -1)) (make-tank (tank-x t) 1)]
        [else  t]))