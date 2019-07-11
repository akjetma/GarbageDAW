(ns garbagedaw.core
  (:require [reagent.core :as reagent]
            [bitwise]))

(enable-console-print!)

; (bitwise/interleaveBytes 255 184 76)
; (bitwise/deInterleavePCM (bitwise/interleaveRGBA (js/parseInt "11111111101110000100110011111111" 2)))

(defn int-to-bit-str 
  [x]
  (.toString x 2))

(defn bit-str-to-int
  [s]
  (js/parseInt s 2))

(def BIT_DEPTH 8)
(def MAX_UINT32 (js/parseInt (.repeat "1" 32) 2))
(def MAX_UINT24 (js/parseInt (.repeat "1" 24) 2))
(def U32_MOD (- MAX_UINT32 MAX_UINT24))

; algorithm a:

(defn img-int->audio-float
  [x]
  (-> x
      (- U32_MOD)   ; ignore alpha channel
      (/ MAX_UINT24) ; divide by max 24-bit number (rgb8)
      (* 2)         ; multiply by 2...
      (- 1)))       ; and shift down by one because audio floats E [-1, 1]

(defn audio-float->img-int
  [x]
  (-> x
      (+ 1)
      (/ 2)
      (* MAX_UINT24)
      (+ U32_MOD)))

; algorithm b:

(defn deinterleave-pcm
  [x]
  (-> x
      (+ 1)
      (/ 2)
      (* MAX_UINT24)
      (bitwise/deInterleavePCM)))

(defn interleave-rgb
  [x]
  (-> x
      (bitwise/interleaveRGBA)
      (/ MAX_UINT24)
      (* 2)
      (- 1)))

; ---

(def simple
  {:img->audio img-int->audio-float
   :audio->img audio-float->img-int})

(def z-order-curve
  {:img->audio interleave-rgb
   :audio->img deinterleave-pcm})

(def chosen-algorithm z-order-curve)
;(def chosen-algorithm simple)

(defn image-array-to-audio-array
  [image-array]
  (js/Float32Array.from (js/Uint32Array. (.-buffer image-array)) (:img->audio chosen-algorithm)))

(defn audio-array-to-image-array
  [audio-array]
  ; (js/console.log audio-array)
  (let [res (js/Uint8ClampedArray. (.-buffer (js/Uint32Array.from audio-array (:audio->img chosen-algorithm))))]
    ; (js/console.log res)
    ; (js/console.log (image-array-to-audio-array res))
    res))

(defonce source-image (reagent/atom nil))
(defonce crop-region (reagent/atom nil))

(defonce audio-ctx (js/window.AudioContext.))
(defonce buff-src (atom nil))

(defn img-data->audio
  [audio-ctx img-data]
  (let [audio-data (image-array-to-audio-array (.-data img-data))
        audio-buffer (.createBuffer audio-ctx 2 (.-length audio-data) (.-sampleRate audio-ctx))]
    (.copyToChannel audio-buffer audio-data 0)
    (.copyToChannel audio-buffer audio-data 1)
    audio-buffer))

(defn play-canvas
  [canvas]
  (let [img-data (.getImageData (.getContext canvas "2d") 0 0 (.-width canvas) (.-height canvas))
        audio-buffer (img-data->audio audio-ctx img-data)
        new-src (.createBufferSource audio-ctx)]
    (set! (.-loop new-src) true)
    (set! (.-buffer new-src) audio-buffer)
    (.connect new-src (.-destination audio-ctx))
    (when @buff-src
      (.stop @buff-src))
    (.start new-src)
    (reset! buff-src new-src)))

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

(defn store-source-audio
  [e]
  (let [file (aget e "target" "files" 0)
        reader (js/FileReader.)]
    (set! (.-onload reader) (fn [e]
                              (let [raw-buf (.. e -target -result)]
                                (.decodeAudioData audio-ctx raw-buf
                                                  (fn [buf]              
                                                    (let [edge-length (js/Math.floor (js/Math.sqrt (.-length buf)))
                                                          audio-array (.subarray (.getChannelData buf 0) 0 (* edge-length edge-length))
                                                          image-array (audio-array-to-image-array audio-array)
                                                          image-data (js/ImageData. image-array edge-length edge-length)
                                                          canvas (js/document.createElement "canvas")]
                                                      (set! (.-width canvas) edge-length)
                                                      (set! (.-height canvas) edge-length)
                                                      (draw-img-data canvas image-data)                                     
                                                      (reset! source-image canvas)
                                                      (reset! crop-region {:x 0 :y 0 :width edge-length :height edge-length})))))))
    (.readAsArrayBuffer reader file)))

(defn store-source
  [e]
  (when (.startsWith (aget e "target" "files" 0 "type") "image/")
    (store-source-img e))
  (when (.startsWith (aget e "target" "files" 0 "type") "audio/")
    (store-source-audio e)))

(defn draw-rect
  [canvas rect]
  (let [ctx (.getContext canvas "2d")]
    (set! (.-strokeStyle ctx) "red")
    (set! (.-lineWidth ctx) 2)
    (.setLineDash ctx (clj->js [5 10]))
    (.strokeRect ctx (:x rect) (:y rect) (:width rect) (:height rect))))

(defn get-cursor-position
  [canvas e]
  (let [elem-rect (.getBoundingClientRect canvas)
        elem-x (- (.-clientX e) (.-left elem-rect))
        elem-y (- (.-clientY e) (.-top elem-rect))
        frac-x (/ elem-x (.-width elem-rect))
        frac-y (/ elem-y (.-height elem-rect))
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
                               (draw-img-data @dom-node))
                          (play-canvas @dom-node)))]

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
   [:input#upload {:type "file" :accept "image/*, audio/*" :on-change store-source}]
   [:div#windows-container
    [:div#crop-window
     [c-crop-canvas]]
    [:div#preview-window
     [c-preview-canvas]]]])

(defn start!
  []
  (reagent/render [app] (js/document.getElementById "root")))

(start!)