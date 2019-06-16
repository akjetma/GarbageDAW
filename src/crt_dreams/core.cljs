(ns crt-dreams.core
  (:require [reagent.core :as reagent]))

(enable-console-print!)

(defonce source-image-data (reagent/atom nil))
(defonce crop-region (reagent/atom {:x 25 :y 25 :width 100 :height 100}))

(defn get-id
  [id]
  (js/document.getElementById id))

(defn img-data->map
  [img-data]
  {:width (.-width img-data)
   :height (.-height img-data)
   :data (.-data img-data)})

(defn map->img-data
  [{:keys [width height data]}]
  (js/ImageData. data width height))

(defn img->img-data
  [img]
  (let [temp-cvs (js/document.createElement "canvas")
        temp-ctx (.getContext temp-cvs "2d")]
    (set! (.-width temp-cvs) (.-width img))
    (set! (.-height temp-cvs) (.-height img))
    (.drawImage temp-ctx img 0 0)
    (.getImageData temp-ctx 0 0 (.-width temp-cvs) (.-height temp-cvs))))

(defn store-source-data
  [e]
  (let [src (js/URL.createObjectURL (aget (.. e -target -files) 0))
        img-tmp (js/document.createElement "img")]
    (set! (.-onload img-tmp) #(reset! source-image-data (img-data->map (img->img-data img-tmp))))
    (set! (.-src img-tmp) src)))

(defn draw-img-data
  [canvas id-map]
  (let [ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (.putImageData ctx (map->img-data id-map) 0 0)))

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
    (.addEventListener canvas "mouseup" on-finish)
    (.addEventListener canvas "mouseleave" on-finish)))

(defn c-crop-canvas
  []
  (let [dom-node (reagent/atom nil)
        on-down (fn [e] (start-crop @dom-node e))
        actual-render (fn []
                        (when @source-image-data
                          (draw-img-data @dom-node @source-image-data))
                        (when @crop-region
                          (draw-rect @dom-node @crop-region)))]
    
    (reagent/create-class 
     {:component-did-update
      (fn [this] (actual-render))
      
      :component-did-mount 
      (fn [this] 
        (reset! dom-node (reagent/dom-node this))
        (actual-render))
      
      :reagent-render
      (fn []
        @crop-region
        [:canvas#crop-canvas (when-let [data @source-image-data]
                               {:width (:width data)
                                :height (:height data)
                                :on-mouse-down on-down})])})))

(defn c-preview-canvas
  []
  [:canvas#preview-canvas])

(defn app 
  []
  [:div#app
   [:input#upload {:type "file" :accept "image/*" :on-change store-source-data}]
   [:div#windows-container
    [:div#crop-window
     [c-crop-canvas]]
    [:div#preview-window
     [c-preview-canvas]]]])

(defn start!
  []
  (reagent/render [app] (get-id "root")))

(start!)