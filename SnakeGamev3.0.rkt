#lang racket
;; Proyecto Final: Fundamentos de Programación (750080M) Semestre 1 (2017-1).
;; Profesor: Andrés Mauricio Castillo Robles.
;; Estudiantes: Juan Marcos Caicedo Mejía (1730504), Santiago Mejía Martínez (1731253).
;;---------------------------------------------------------------------------------------
(require 2htdp/universe)
(require 2htdp/image)
(require racket/local)
(require 2htdp/batch-io)
(require htdp/gui)
;;---------------------------------------------------------------------------------------
;; Constantes y definiciones iniciales
;;---------------------------------------------------------------------------------------
(define-struct fullgame (snake snake_2 foods foods_2))
 
(define-struct snake (dir parts))
 
(define-struct posn (x y) #:transparent)
 
(define-struct food (pos expir_tm))
 
(define exp_time 5)
 
(define canvas_regular_size 30)
 
(define max_food 3)
(define max_food_2 1)
(define number_of_new_food 3)
(define number_of_new_food_2 2)

(define ontick_time 0.08)
 
(define parts_size 20)

(define snake_img (square parts_size "solid" "white"))
(define snake_2_img (square parts_size "solid" "cyan"))

(define food_img (square parts_size "solid" "red"))
(define food_2_img (square parts_size "solid" "magenta"))
 
(define (accountant lista)
   (- (length lista) 3))

(define (snake_head sn)
  (first (snake-parts sn)))

(define (snake_body sn)
  (rest (snake-parts sn)))

(define (snake_tail sn)
  (last (snake-parts sn)))

(define (snake_change_direction sn d)
  (make-snake d (snake-parts sn)))
;;---------------------------------------------------------------------------------------
;; FUNCIÓN GENERAL TODRAW
;;---------------------------------------------------------------------------------------

(define canvas (empty-scene (* canvas_regular_size 20) (* canvas_regular_size 20) "black"))

;; La Función General TODRAW posee unas cuantas definiciones y funciones locales:
;; 1. draw_food_in_canvas: Su tarea es dibujar las imagenes de manera recursiva en el Canvas
;; 2. get_posns_from_food: De la lista de comidas, extrae los Posn's recursivamente
;; 3. draw_foods: La función que llama la función 1
;; 4. draw_snake_in_scene: Dibuja la serpiente en la escena dada, parte por parte.
;; 5. draw_snake: Dibuja todos los elementos como contador y nombre de jugador, junto con las funciones anteriores
 
(define (todraw_mainFunction w)
  (local ((define (draw_food_in_canvas w)(local ((define (draw_food_list_in_scene foods foods_2 scene)
                                                   (define (get_posns_from_food foods)
                                                     (cond
                                                       [(empty? foods) empty]
                                                       [else (cons (food-pos (first foods))
                                                                   (get_posns_from_food (rest foods)))]))
                                                   (draw_img_list_in_scene (get_posns_from_food foods_2) food_2_img (draw_img_list_in_scene (get_posns_from_food foods) food_img scene)))
                                                 (define (draw_foods w)
                                                   (draw_food_list_in_scene (fullgame-foods w) (fullgame-foods_2 w) canvas)))
                                           (draw_foods w)))
          (define (draw_snake_in_scene snake part scene)
            (define snake_body_scene
              (draw_img_list_in_scene (snake_body snake) part scene)) 
            (draw_img_in_scene (snake_head snake) part snake_body_scene))
          (define (draw_snake w)
            (cond [(empty? (fullgame-snake_2 w))
                   (place-image (text (text-contents nombre?) 25 "white") 50 20
                                (place-image (text "Score: " 25 "white") 170 20
                                             (place-image (text (number->string (accountant (snake-parts (fullgame-snake w)))) 25 "white") 220 20
                                                          (draw_snake_in_scene (fullgame-snake w) snake_img
                                                                               (draw_food_in_canvas w)))))]
                  [else
                   (place-image (text (text-contents nombre1?) 25 "white") 50 20
                                (place-image (text "Score: " 25 "white") 170 20
                                             (place-image (text (number->string (accountant (snake-parts (fullgame-snake w)))) 25 "white") 220 20
                                                          (draw_snake_in_scene (fullgame-snake w) snake_img
                                                                               (draw_snake_in_scene (fullgame-snake_2 w) snake_2_img
                                                                                                    (place-image (text (text-contents nombre2?) 25 "white") 350 20
                                                                                                                 (place-image (text "Score: " 25 "white") 470 20
                                                                                                                              (place-image (text (number->string (accountant (snake-parts (fullgame-snake_2 w)))) 25 "white") 520 20
                                                                                                                                           (draw_food_in_canvas w)))))))))])))
    (draw_snake w)))
                                                                                       
                                                                                       
                                   
 
 
 
;;---------------------------------------------------------------------------------------
;; FUNCIÓN GENERAL WHO WOULD WIN?
;;---------------------------------------------------------------------------------------
;; Esta función también posee algunas definiciones locales, como:
;; 1. snake: Se refiere a la serpiente 1.
;; 2. snake_2: Se refiere a la serpiente 2.
;; 3. gameover_text: Texto que se muestra cuando el juego acaba.
;; 4. winner: Función que usa las funciones anteriores para ejecutarlas de manera conjunta y local.
;; Es la función que utiliza operaciones para determinar cual de las dos serpientes, o si es solo una serpiente
;; muestra el puntaje obtenido.

(define (who_would_win? w)
  (local
  ((define snake (fullgame-snake w))
   
  (define snake_2 (fullgame-snake_2 w))
  
  (define (gameover_text w snake text1)
  (overlay (text "Game Over" 25 "white")
           (place-image
            (text text1 15 "white") 300 320
            (place-image
             (text (number->string (accountant (snake-parts snake))) 20 "gray") 
             350 320
             (todraw_mainFunction w)))))
  
  (define (winner w)
  (cond [(empty? snake_2)
         (gameover_text w snake "Final Score: ")]
         
        [else (cond [(> (accountant (snake-parts snake)) (accountant (snake-parts snake_2))) (gameover_text w snake (string-append (text-contents nombre1?) " Wins : "))]
                    [(< (accountant (snake-parts snake)) (accountant (snake-parts snake_2))) (gameover_text w snake_2 (string-append (text-contents nombre2?) " Wins : "))]
                    [else (gameover_text w snake "Draw : ")])])))
  (winner w)))

;;---------------------------------------------------------------------------------------
;; FUNCIÓN GENERAL ONTICK
;;---------------------------------------------------------------------------------------
;; Es la función más compleja del juego, ya que se encarga de tanto los movimientos como el hecho de crecer.
;; Tiene variadas definiciones locales que ayudan a su rendimiento:
;; 1. decrease_and_generate_new_foods_in_world: Función que utiliza algunas funciones definidas localmente
;; para decrementar el tiempo de vida de las comidas e ir generando un cierto numero de nuevas comidas.
;; 2. renew_foods_of_list: Función que va renovando las comidas de la lista
;; 3. is_expirtime_zero?: Verifica si el tiempo de expiración de cierta comida es cero.
;; 4. renew_foods: Renueva de manera individual y recursiva las comidas de la lista.
;; 5. decrease_and_generate_new_foods: De manera individual decrece el tiempo de cada food y va generando nuevas
;; 6. new_world_OnTick: Función General que utiliza las funciones anteriores para el proceso de movimiento y comer.
;; 7. the_snake_can_eat?: Con funciones auxiliares realiza la acción de determinar si las serpiente puede comer un food dado
;; 8. is_snake_near_of_food?: Determina si la serpiente está cerca de alguna Food de la lista
;; 9. is_near?: Pregunta si un objeto está cerca de otro
;; 10. can_eat?: Pregunta si la serpiente puede comer en el momento dado
;; 11. eat: Función comer, se encarga de remover una comida que está siendo comida de la lista de comidas.
       
(define (ontick_mainFunction w)
  (local ((define (decrease_and_generate_new_foods_in_world foods number_of_new_foods_to_generate)
            (local ((define (renew_foods_of_list foods n)
                      (local ((define (is_expirtime_zero? f) 
                                (zero? (food-expir_tm f)))
                              (define (renew_foods foods n)
                                (cond
                                  [(empty? foods) empty]
                                  [(is_expirtime_zero? (first foods)) 
                                   (append (generate_list_of_n_food (random n)) (renew_foods (rest foods) n))]
                                  [else (cons (first foods) (decrease_time_all_foods_of_list (rest foods)))])))
                        (renew_foods foods n))) 
                    (define (decrease_and_generate_new_foods foods number_of_new_foods_to_generate) 
                      (cond [(empty? foods) (generate_list_of_n_food (add1 (random (sub1 number_of_new_foods_to_generate))))]
                            [else (decrease_time_all_foods_of_list (renew_foods_of_list foods number_of_new_foods_to_generate))])))
              (decrease_and_generate_new_foods foods number_of_new_foods_to_generate)))
          (define (new_world_OnTick w)
            (cond [(fullgame? w)
                   (define (the_snake_can_eat? snake foods)
                     (local ((define (is_snake_near_of_food? s f)
                               (local ((define (posn=? p1 p2)
                                         (and (= (posn-x p1) (posn-x p2))
                                              (= (posn-y p1) (posn-y p2))))
                                       (define (is_near? s f) 
                                         (posn=? s (food-pos f))))
                                 (is_near? s f)))
                             (define (can_eat? snake foods)
                               (cond
                                 [(empty? foods) #f]
                                 [else (cond
                                         [(is_snake_near_of_food? (snake_head snake) (first foods))
                                          (first foods)]
                                         [else (can_eat? snake (rest foods))])])))
                       (can_eat? snake foods)))
                   (define (eat foods food_that_is_going_to_be_eaten) 
                     (remove food_that_is_going_to_be_eaten foods))
                   (define snake (fullgame-snake w))
                   (define snake_2 (fullgame-snake_2 w))
                   (define foods (fullgame-foods w))
                   (define foods_2 (fullgame-foods_2 w))
                   (define food_to_eat1 (the_snake_can_eat? snake foods))
                   (define food_to_eat1_2 (the_snake_can_eat? snake foods_2))
                   (cond [(empty? snake_2) 
                          (cond [food_to_eat1
                                 (make-fullgame (keep_growing_snake snake 1) 
                                                snake_2 
                                                (decrease_and_generate_new_foods_in_world (eat foods food_to_eat1) number_of_new_food) 
                                                (decrease_and_generate_new_foods_in_world (eat foods_2 food_to_eat1_2) number_of_new_food_2))]
                                
                                [food_to_eat1_2
                                 (make-fullgame (keep_growing_snake snake 2) 
                                                snake_2 
                                                (decrease_and_generate_new_foods_in_world (eat foods food_to_eat1) number_of_new_food) 
                                                (decrease_and_generate_new_foods_in_world (eat foods_2 food_to_eat1_2) number_of_new_food_2))]
                                [else (make-fullgame (keep_going_snake snake) snake_2 (decrease_and_generate_new_foods_in_world foods number_of_new_food) (decrease_and_generate_new_foods_in_world foods_2 number_of_new_food_2))])]
                         
                         [else
                          (define food_to_eat2 (the_snake_can_eat? snake_2 foods))
                          (define food_to_eat2_2 (the_snake_can_eat? snake_2 foods_2))
                          (cond [food_to_eat1
                                 (make-fullgame (keep_growing_snake snake 1) 
                                                (keep_going_snake snake_2) 
                                                (decrease_and_generate_new_foods_in_world (eat foods food_to_eat1) number_of_new_food) 
                                                (decrease_and_generate_new_foods_in_world (eat foods_2 food_to_eat1_2) number_of_new_food_2))]
                                
                                [food_to_eat1_2
                                 (make-fullgame (keep_growing_snake snake 2) 
                                                (keep_going_snake snake_2) 
                                                (decrease_and_generate_new_foods_in_world (eat foods food_to_eat1) number_of_new_food) 
                                                (decrease_and_generate_new_foods_in_world (eat foods_2 food_to_eat1_2) number_of_new_food_2))]
                                
                                [food_to_eat2
                                 (make-fullgame (keep_going_snake snake) 
                                                (keep_growing_snake snake_2 1) 
                                                (decrease_and_generate_new_foods_in_world (eat foods food_to_eat2) number_of_new_food)
                                                (decrease_and_generate_new_foods_in_world (eat foods_2 food_to_eat2_2) number_of_new_food_2))]
                                
                                [food_to_eat2_2
                                 (make-fullgame (keep_going_snake snake) 
                                                (keep_growing_snake snake_2 2) 
                                                (decrease_and_generate_new_foods_in_world (eat foods food_to_eat2) number_of_new_food) 
                                                (decrease_and_generate_new_foods_in_world (eat foods_2 food_to_eat2_2) number_of_new_food_2))]
                                
                                [else (make-fullgame (keep_going_snake snake) (keep_going_snake snake_2) (decrease_and_generate_new_foods_in_world foods number_of_new_food) (decrease_and_generate_new_foods_in_world foods_2 number_of_new_food_2))])])]
                  [else w])))
    (new_world_OnTick w)))
;;---------------------------------------------------------------------------------------
;; FUNCIÓN GENERAL ONKEY
;;---------------------------------------------------------------------------------------
;; Función General Onkey, que se encarga de recibir acciones del teclado. Posee también funciones localmente definidas:
;; 1. what_direction?: Pregunta que tecla se está oprimiendo.
;; 2. general_change_direction: Función general que cambia la dirección de la Serpiente.
;; 3. opposite_direction?: Teniendo en cuenta una dirección dada, determina la dirección opuesta a ella.
;; 4. world_change_dir: Cambia la dirección de las serpientes en el mundo como tal.
(define (onkey_mainFunction w k)
  (local
    ((define (what_direction? x)
       (or (key=? x "up") (key=? x "w") (key=? x "W")
           (key=? x "down") (key=? x "a") (key=? x "S")
           (key=? x "left") (key=? x "s") (key=? x "A")
           (key=? x "right") (key=? x "d") (key=? x "D")))
     (define (general_change_direction w d)
       (local ((define (opposite_direction? d1 d2)
                 (cond
                   [(string=? d1 "up") (string=? d2 "down")]
                   [(string=? d1 "down") (string=? d2 "up")]
                   [(string=? d1 "left") (string=? d2 "right")]
                   [(string=? d1 "right") (string=? d2 "left")]
                   [(or (string=? d1 "w") (string=? d1 "W")) (string=? d2 "s")]
                   [(or (string=? d1 "s") (string=? d1 "S")) (string=? d2 "w")]
                   [(or (string=? d1 "a") (string=? d1 "A")) (string=? d2 "d")]
                   [(or (string=? d1 "d") (string=? d1 "D")) (string=? d2 "a")]))
               (define (world_change_dir w d)
                 (define snake (fullgame-snake w))
                 (define snake_2 (fullgame-snake_2 w))
                 (cond [(empty? snake_2)
                        (cond
                          [(and (cons? (rest (snake-parts snake)))
                                (opposite_direction? (snake-dir snake) d)) w]
                 
                          [else (make-fullgame (snake_change_direction snake d)
                                               snake_2
                                               (fullgame-foods w)
                                               (fullgame-foods_2 w))])]
                       [else (cond 
                               [(and (cons? (rest (snake-parts snake_2)))
                                     (opposite_direction? (snake-dir snake_2) d)) w]
                               [(and (cons? (rest (snake-parts snake)))
                                     (opposite_direction? (snake-dir snake) d)) w]
                      
                               [else (cond [(or (string=? d "w") (string=? d "W")
                                                (string=? d "s") (string=? d "S")
                                                (string=? d "a") (string=? d "A")
                                                (string=? d "d") (string=? d "D"))
                                            (make-fullgame snake
                                                           (snake_change_direction snake_2 d)
                                                           (fullgame-foods w)
                                                           (fullgame-foods_2 w))]
                                           [else (make-fullgame (snake_change_direction snake d)
                                                                snake_2
                                                                (fullgame-foods w)
                                                                (fullgame-foods_2 w))])])])))
         (world_change_dir w d)))
          (define (direction_snake w k)
            (cond [(what_direction? k) (general_change_direction w k)]
                  [else w])))
    (direction_snake w k)))
;;---------------------------------------------------------------------------------------

;; Función keep_going_snake: snake -> snake
;; Función que se encarga de permitir el movimiento perpetúo de la serpiente, con algunas definiciones locales:
;; 1. erase_last: Elimina el ultimo elemento de una lista.
;; 2. next_snakehead: Cada vez que se mueve y NO COME, borra el ultimo elemento de la lista y lo pone de primero.
;; 3. head: Cabeza de la serpiente
;; 4. dir: Dirección de la serpiente.
;; 5. go_on: Función que ejecuta ls funciones anteriores, es la función global de keep_going_snake.
(define (keep_going_snake sn)
  (local ((define (erase_last parts) 
           (cond
             [(empty? (rest parts)) empty]
             [else (cons (first parts) 
                (erase_last (rest parts)))]))
          (define (next_snakehead sn)
          (define head (snake_head sn))
          (define dir (snake-dir sn))
            (cond
              [(or (string=? dir "up") (string=? dir "w") (string=? dir "W")) (posn-move head 0 -1)]
              [(or (string=? dir "down") (string=? dir "s") (string=? dir "S")) (posn-move head 0 1)]
              [(or (string=? dir "left") (string=? dir "a") (string=? dir "A")) (posn-move head -1 0)]
              [(or (string=? dir "right") (string=? dir "d") (string=? dir "D")) (posn-move head 1 0)]))
          (define (go_on sn) 
            (make-snake (snake-dir sn) 
                        (cons (next_snakehead sn) (erase_last (snake-parts sn))))))
  (go_on sn)))

;;---------------------------------------------------------------------------------------

;; Función keep_growing_snake: snake -> snake
;; Función que encapsula las funciones auxiliares que colaboran a la función global "grow" que permite el crecimiento de la serpiente
;; 1. next_snaketail: Coge la cola de la serpiente y la duplica, simulando el efecto de crecer.
;; 2. tail: Cola de la serpiente
;; 3. dir: Dirección de la serpiente.
;; 4. grow: Función global de crecimiento, se usó polimorfismo en ella para que sea una función que haga
;; que la serpiente crezca N veces de tamaño. Usa recursión también. Se asemeja a un ciclo. (Hasta que n no sea cero no deja
;; de crecer).
(define (keep_growing_snake sn n)
  (local ((define (next_snaketail sn)
         (define tail (snake_tail sn))
         (define dir (snake-dir sn))
            (cond
              [(or (string=? dir "up") (string=? dir "w") (string=? dir "W")) (posn-move tail 0 1)]
              [(or (string=? dir "down") (string=? dir "s") (string=? dir "S")) (posn-move tail 0 -1)]
              [(or (string=? dir "left") (string=? dir "a") (string=? dir "A")) (posn-move tail 1 0)]
              [(or (string=? dir "right") (string=? dir "d") (string=? dir "D")) (posn-move tail -1 0)]))
          (define (grow sn n) 
            (cond [(zero? n) sn]
                  [else (grow (snake (snake-dir sn) 
                                     (append (snake-parts sn) (list (next_snaketail sn)))) (- n 1))])))
    (grow sn n)))

;;---------------------------------------------------------------------------------------

;; Función posn-move: posn, dx, dy -> posn
;; Función que se encarga de modificar un posn aumentandole un dx y un dy en sus componentes X y Y.
(define (posn-move p dx dy) 
  (posn (+ (posn-x p) dx) 
        (+ (posn-y p) dy)))

;;---------------------------------------------------------------------------------------

;; Función decrease_time_all_foods_of_list: list of foods -> list of foods
;; Función que colabora al Ontick, se encarga de manera recursiva irle disminuyendo en uno a cada food de la lista
;; a medida que pasa el tiempo.
(define (decrease_time_all_foods_of_list foods)
  (local ((define (decrease_in_1_expirtime_of_food foods)
          (make-food (food-pos foods) (sub1 (food-expir_tm foods))))
        (define (decrease_expirtime_all_foods_of_list foods)
          (cond [(empty? foods) empty]
                [else (cons (decrease_in_1_expirtime_of_food (first foods)) 
                            (decrease_expirtime_all_foods_of_list (rest foods)))])))
  (decrease_expirtime_all_foods_of_list foods)))

;;---------------------------------------------------------------------------------------

;; Función generate_list_of_n_food: number -> list
;; Genera una lista de N comidas. Utiliza localmente la función new_food que es la función que crea una nueva comida con
;; POSN's aleatorios.
;; food_list es la lista que se genera utilizando el comando build-list y se le aplica un procedimiento
;; en el cual se usa lambda para invocar una función anónima, cuya tarea es generar la lista de elementos con la
;; misma orden: new_food
;; list_of_n_food es la función que va construyendo las listas de manera recursiva
(define (generate_list_of_n_food number_of_foods_to_generate)
  (local ((define (new_food) 
              (make-food (posn (+ (random (- canvas_regular_size 2)) 2)
                               (+ (random (- canvas_regular_size 2)) 2))
                         (+ 20 (* 2 (random exp_time) (random exp_time)))))
            (define food_list (build-list number_of_foods_to_generate (lambda (x) (new_food))))
            (define (list_of_n_food number_of_foods_to_spawn)
            (cond [(= number_of_foods_to_spawn 0) empty]
                  [else (cons (list-ref food_list  (sub1 number_of_foods_to_spawn)) 
                    (list_of_n_food (sub1 number_of_foods_to_spawn)))])))
    (list_of_n_food number_of_foods_to_generate)))

;;---------------------------------------------------------------------------------------

;; Función draw_img_list_in_scene: posns, img, scene -> image
;; Función que dibuja de manera recursiva imagenes correspondientes a una lista en POSN's fijos.
(define (draw_img_list_in_scene posns img scene)
  (cond
    [(empty? posns) scene]
    [else (draw_img_in_scene (first posns)
               img
               (draw_img_list_in_scene (rest posns) img scene))]))

;;--------------------------------------------------------------------------------------

;; Función draw_img_in_scene: posn, img, scene -> image
;; Dibuja una imagen en un POSN dado de en una escena.
(define (draw_img_in_scene posn img scene)
  (place-image img
               (* (posn-x posn) parts_size)
               (* (posn-y posn) parts_size)
               scene))

;;---------------------------------------------------------------------------------------
;; FUNCIÓN GENERAL STOP-WHEN
;;---------------------------------------------------------------------------------------

;; Consiste de 3 funciones localmente definidas:
;; 1. is_snake_colliding_with_itself?: snake -> boolean
;; Retorna TRUE si se está chocando la Culebra con ella misma. De lo contrario FALSE.
;; 2. are_the_snakes_colliding?
;; Verifica que los elementos de la lista de POSN's de las serpientes no estén contenidos en ellos mismos
;; (que las serpientes choquen). De chocar retorna TRUE
;; 3. is_snake_colliding_with_walls?: snake -> boolean
;; Siempre revisa que la Snake no esté tocando las paredes, de ser así, lanza TRUE y todo para.
(define (stopwhen_mainFunction w)
  (local ((define (is_snake_colliding_with_itself? snake)
            (list? (member (snake_head snake) (snake_body snake))))
          (define (are_the_snakes_colliding? snake snake_2)
            (or (cons? (member (snake_head snake) (snake_body snake_2)))
                (cons? (member (snake_head snake_2) (snake_body snake)))))
          (define (is_snake_colliding_with_walls? snake)
            (define x (posn-x (snake_head snake)))
            (define y (posn-y (snake_head snake)))
            (or (= 0 x) (= x canvas_regular_size)
                (= 0 y) (= y canvas_regular_size)))
          (define (is_snake_dead? w)
            (define snake (fullgame-snake w))
            (define snake_2 (fullgame-snake_2 w))
            (cond [(empty? snake_2) (or (is_snake_colliding_with_walls? snake) (is_snake_colliding_with_itself? snake))]
                  [else (or (is_snake_colliding_with_walls? snake)
                            (is_snake_colliding_with_walls? snake_2)
                            (is_snake_colliding_with_itself? snake)
                            (is_snake_colliding_with_itself? snake_2)
                            (are_the_snakes_colliding? snake snake_2)
                  )])))
    (is_snake_dead? w)))

;;---------------------------------------------------------------------------------------

;; Big-Bang que ejecuta para 1 solo jugador
(define (playgame e)
  (big-bang (make-fullgame (make-snake "right" (list (posn 1 1) (posn 1 2) (posn 1 3))) empty (generate_list_of_n_food max_food) (generate_list_of_n_food max_food_2))
          (on-tick ontick_mainFunction ontick_time)
          (on-key onkey_mainFunction)
          (to-draw todraw_mainFunction)
          (stop-when stopwhen_mainFunction who_would_win?)
          (name "La culebra v 3.0")) #t)

;;---------------------------------------------------------------------------------------

;; Big-Bang que ejecuta para 2 jugadores
(define (playgame2 e)
  (big-bang (make-fullgame (make-snake "right" (list (posn 1 1) (posn 1 2) (posn 1 3))) (make-snake "d" (list (posn 10 1) (posn 10 2) (posn 10 3))) (generate_list_of_n_food max_food) (generate_list_of_n_food max_food_2))
          (on-tick ontick_mainFunction ontick_time)
          (on-key onkey_mainFunction)
          (to-draw todraw_mainFunction)
          (stop-when stopwhen_mainFunction who_would_win?)
          (name "La culebra v 3.0")) #t)

;;---------------------------------------------------------------------------------------

;; Funciones referentes a GUI
 
(define titulo
  (make-message "
         Snake Game
"))
 
(define instrucciones
  (make-message "                       Cómo jugar(Primer jugador):
      Se mueve con las flechas direccionales del teclado,
      si se choca con ella misma, el otro jugador o las paredes, pierde."))
 
(define instrucciones2
  (make-message "                       Cómo jugar(Segundo jugador):
      Se mueve con las teclas (W,A,S,D) del teclado,
      si se choca con ella misma, el otro jugador o las paredes, pierde."))
 
 
(define nombre?
  (make-text "(Jugador 1)Digite su nombre"))
 
(define nombre1?
  (make-text "(Jugador 1)Digite su nombre"))
 
(define nombre2?
  (make-text "(Jugador 2)Digite su nombre"))
 
 
(define (w1 e)
    (create-window
      (list
       (list nombre?)
       (list (make-button "Jugar" playgame))))#t)
 
(define (w2 e)
    (create-window
      (list
       (list nombre1?)
       (list nombre2?)
       (list (make-button "Jugar" playgame2))))#t)
 
(define (w3 e)
  (create-window
   (list
    (list instrucciones)
    (list instrucciones2)))#t)
 
 
 
(define w
    (create-window
      (list
       (list titulo)
       (list (make-button "Un jugador" w1))
       (list (make-button "Dos jugadores" w2))
       (list (make-button "Cómo jugar" w3))
       (list (make-button "Salir" (lambda (e) (hide-window w)))))))

;;---------------------------------------------------------------------------------------
