(ns sicp.lecture.sketch
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn update-state [state]
  {:color (:color state)
   :angle (+ (:angle state) 0.1)})

(defn update-state [state]
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (:angle state)})

(defn update-state [state]
  {:color (:color state)
   :angle (:angle state)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
                        ; Draw the circle.
                        (q/rect (- x 50) (- y 50) 100 100))))

(defn show-frame-rate [options]
  (let [; retrieve existing draw function or use empty one if not present
        draw (:draw options (fn []))
        ; updated-draw will replace draw
        updated-draw (fn []
                       (draw) ; call user-provided draw function
                       (q/fill 0)
                       (q/text-num (q/current-frame-rate) 10 10))]
    ; set updated-draw as :draw function
    (assoc options :draw updated-draw)))

(q/defsketch my-sketch
             :title "You spin my circle right round"
             :size [500 500]
             ; setup function called only once, during sketch initialization.
             :setup setup
             ; update-state is called on each iteration before draw-state.
             :update update-state
             :draw draw-state
             :features [:keep-on-top]
             ; This sketch uses functional-mode middleware.
             ; Check quil wiki for more info about middlewares and particularly
             ; fun-mode.
             :middleware [show-frame-rate m/fun-mode])