(ns crt-dreams.core
  (:require [reagent.core :as reagent]))

(enable-console-print!)

(def MAX_UINT32 4294967295)
(def U32_MOD 4278190080)
(def U32_RANGE (- MAX_UINT32 U32_MOD))

(defn img-int->audio-float
  [x]
  (-> x
      (- U32_MOD)   ; ignore alpha channel
      (/ U32_RANGE) ; divide by max 24-bit number (rgb8)
      (* 2)         ; multiply by 2...
      (- 1)))       ; and shift down by one because audio floats E [-1, 1]

(defn audio-float->img-int
  [x]
  (-> x
      (+ 1)
      (/ 2)
      (* U32_RANGE)
      (+ U32_MOD)))

(defn image-array-to-audio-array
  [image-array]
  (js/Float32Array.from (js/Uint32Array. (.-buffer image-array)) img-int->audio-float))

(defn audio-array-to-image-array
  [audio-array]
  (js/Uint8ClampedArray. (.-buffer (js/Uint32Array.from audio-array audio-float->img-int))))

(defonce source-image (reagent/atom nil))
(defonce crop-region (reagent/atom nil))

(def pixels-per-second 1)
(def num-channels 2)

(defn img-data->audio-old
  [audio-ctx img-data]
  (let [frames-per-pixel (js/Math.floor (/ (.-sampleRate audio-ctx) pixels-per-second))
        audio-buffer (.createBuffer audio-ctx num-channels 
                                    (* frames-per-pixel (.-width img-data) (.-height img-data)) 
                                    (.-sampleRate audio-ctx))]
    (doseq [ch-i (range num-channels)
            :let [channel-buffer (.getChannelData audio-buffer ch-i)]]
      (doseq [px-i (range (* (.-width img-data) (.-height img-data)))
              :let [px-v (aget (.-data img-data) (* 4 px-i))
                    au-v (- (* (/ px-v 255) 2) 1)]]
        (.fill channel-buffer au-v 
               (* px-i frames-per-pixel)
               (* (inc px-i) frames-per-pixel))))
    (js/console.log img-data)
    (js/console.log audio-buffer)
    audio-buffer))

(defn img-data->audio
  [audio-ctx img-data]
  (let [audio-data (image-array-to-audio-array (.-data img-data))
        audio-buffer (.createBuffer audio-ctx 2 (.-length audio-data) (.-sampleRate audio-ctx))]
    (.copyToChannel audio-buffer audio-data 0)
    (.copyToChannel audio-buffer audio-data 1)
    (js/console.log audio-buffer)
    audio-buffer))

(defn play-canvas
  [canvas]
  (let [img-data (.getImageData (.getContext canvas "2d") 0 0 (.-width canvas) (.-height canvas))
        audio-ctx (js/window.AudioContext.)
        audio-buffer (img-data->audio audio-ctx img-data)
        src (.createBufferSource audio-ctx)]
    (set! (.-buffer src) audio-buffer)
    (.connect src (.-destination audio-ctx))
    (.start src)))

(defn draw-img
  [canvas img]
  (let [ctx (.getContext canvas "2d")]
    (.drawImage ctx img 0 0)))

(defn draw-img-data
  [canvas img-data]
  (let [ctx (.getContext canvas "2d")]
    (.putImageData ctx img-data 0 0)))

(defn img->img-data
  ([img] (img->img-data img {:x 0 :y 0 :width (.-width img) :height (.-height img)}))
  ([img {:keys [x y width height]}]
   (let [temp-cvs (js/document.createElement "canvas")
         temp-ctx (.getContext temp-cvs "2d")]
     (set! (.-width temp-cvs) (.-width img))
     (set! (.-height temp-cvs) (.-height img))
     (draw-img temp-cvs img)
     (.getImageData temp-ctx x y width height))))

(defn desaturate
  [img-data]
  (let [input-a (.-data img-data)
        length (* (.-width img-data) (.-height img-data) 4)
        output-a (js/Uint8ClampedArray. length)]
    (doseq [i (range 0 length 4)]
      (let [[r g b a :as x] (array-seq (.slice input-a i (+ i 4)))
            avg (js/Math.round (/ (+ r g b) 3))]
        (aset output-a (+ i 0) avg)
        (aset output-a (+ i 1) avg)
        (aset output-a (+ i 2) avg)
        (aset output-a (+ i 3) (js/Math.min a 254))))

    (js/ImageData. output-a (.-width img-data) (.-height img-data))))

(defn store-source-img
  [e]
  (let [src (js/URL.createObjectURL (aget e "target" "files" 0))
        img (js/document.createElement "img")]
    (set! (.-onload img) (fn [e]
                           (reset! source-image img)
                           (reset! crop-region {:x 0 :y 0 :width (.-width img) :height (.-height img)})))
    (set! (.-src img) src)))

(defn draw-rect
  [canvas rect]
  (let [ctx (.getContext canvas "2d")]
    (set! (.-strokeStyle ctx) "red")
    (set! (.-lineWidth ctx) 2)
    (.setLineDash ctx (clj->js [5 10]))
    (.strokeRect ctx (:x rect) (:y rect) (:width rect) (:height rect))))

(defn get-cursor-position
  [canvas e]
  (let [display-rect (.getBoundingClientRect canvas)
        display-x (- (.-clientX e) (.-left display-rect))
        display-y (- (.-clientY e) (.-top display-rect))
        frac-x (/ display-x (.-width display-rect))
        frac-y (/ display-y (.-height display-rect))
        img-x (* frac-x (.-width canvas))
        img-y (* frac-y (.-height canvas))]
    {:x (js/Math.round img-x)
     :y (js/Math.round img-y)}))

(defn update-crop
  [canvas e]
  (let [{:keys [x y]} (get-cursor-position canvas e)]
    (swap! crop-region assoc
           :width (- x (:x @crop-region))
           :height (- y (:y @crop-region)))))

(defn start-crop
  [canvas e]
  (let [{:keys [x y]} (get-cursor-position canvas e)
        on-move (fn [e] (update-crop canvas e))
        on-finish (fn [e] (.removeEventListener canvas "mousemove" on-move))]
    (reset! crop-region {:x x :y y :width 0 :height 0})
    (.addEventListener canvas "mousemove" on-move)
    (.addEventListener canvas "mouseup" on-finish {:once true})
    (.addEventListener canvas "mouseleave" on-finish {:once true})))

(defn c-crop-canvas
  []
  (let [dom-node (reagent/atom nil)
        on-down (fn [e] (start-crop @dom-node e))
        actual-render (fn []
                        (when @source-image
                          (draw-img @dom-node @source-image)
                          (when @crop-region
                            (draw-rect @dom-node @crop-region))))]

    (reagent/create-class
     {:component-did-update actual-render

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        (actual-render))

      :reagent-render
      (fn []
        @crop-region
        [:canvas#crop-canvas (when-let [image @source-image]
                               {:width (.-width image)
                                :height (.-height image)
                                :on-mouse-down on-down})])})))

(defn c-preview-canvas
  []
  (let [dom-node (reagent/atom nil)
        actual-render (fn []
                        (when (and @source-image @crop-region
                                   (not= 0 (* (:width @crop-region) (:height @crop-region))))
                          (->> @crop-region
                               (img->img-data @source-image)
                              ;  (desaturate)
                               (draw-img-data @dom-node))))]

    (reagent/create-class
     {:component-did-update actual-render

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        (actual-render))

      :reagent-render
      (fn []
        @source-image
        [:canvas#preview-canvas (when-let [{:keys [width height]} @crop-region]
                                  {:width (js/Math.abs width)
                                   :height (js/Math.abs height)
                                   :on-click #(play-canvas @dom-node)})])})))

(defn app
  []
  [:div#app
   [:input#upload {:type "file" :accept "image/*" :on-change store-source-img}]
   [:div#windows-container
    [:div#crop-window
     [c-crop-canvas]]
    [:div#preview-window
     [c-preview-canvas]]]])

(defn start!
  []
  (reagent/render [app] (js/document.getElementById "root")))

(start!)