;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.stroke
  (:require
   [app.common.colors :as clr]
   [app.common.data :as d]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.colors :as dc]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.components.numeric-input :refer [numeric-input]]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.options.rows.color-row :refer [color-row]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(def stroke-attrs
  [:strokes
   :stroke-style
   :stroke-alignment
   :stroke-width
   :stroke-color
   :stroke-color-ref-id
   :stroke-color-ref-file
   :stroke-opacity
   :stroke-color-gradient
   :stroke-cap-start
   :stroke-cap-end])

(defn- width->string [width]
  (if (= width :multiple)
   ""
   (str (or width 1))))

(defn- enum->string [value]
  (if (= value :multiple)
    ""
    (pr-str value)))

(defn- stroke-cap-names []
  [[nil             (tr "workspace.options.stroke-cap.none")           false]
   [:line-arrow     (tr "workspace.options.stroke-cap.line-arrow")     true]
   [:triangle-arrow (tr "workspace.options.stroke-cap.triangle-arrow") false]
   [:square-marker  (tr "workspace.options.stroke-cap.square-marker")  false]
   [:circle-marker  (tr "workspace.options.stroke-cap.circle-marker")  false]
   [:diamond-marker (tr "workspace.options.stroke-cap.diamond-marker") false]
   [:round          (tr "workspace.options.stroke-cap.round")          true]
   [:square         (tr "workspace.options.stroke-cap.square")         false]])

(defn- value->name [value]
  (if (= value :multiple)
    "--"
    (-> (d/seek #(= (first %) value) (stroke-cap-names))
        (second))))

(defn- value->img [value]
  (when (and value (not= value :multiple))
    (str "images/cap-" (name value) ".svg")))

(mf/defc stroke-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["ids" "values" "type" "show-caps"]))]}
  [{:keys [ids type values show-caps] :as props}]
  (let [label (case type
                :multiple (tr "workspace.options.selection-stroke")
                :group (tr "workspace.options.group-stroke")
                (tr "workspace.options.stroke"))

        start-caps-state (mf/use-state {:open? false
                                        :top 0
                                        :left 0})
        end-caps-state   (mf/use-state {:open? false
                                        :top 0
                                        :left 0})

        current-stroke-color {:color (:stroke-color values)
                              :opacity (:stroke-opacity values)
                              :id (:stroke-color-ref-id values)
                              :file-id (:stroke-color-ref-file values)
                              :gradient (:stroke-color-gradient values)}

        handle-change-stroke-color
        (mf/use-callback
         (mf/deps ids)
         (fn [index]
           (fn [color]
             (println "-> handle-change-stroke-color" color)
             (st/emit! (dc/change-stroke ids color index)))))


        handle-remove
        (mf/use-callback
         (mf/deps ids)
         (fn [index]
           (fn []
             (println "-> handle-remove" index)
             (st/emit! (dc/remove-stroke ids index)))))

        handle-remove-remove-all
        (fn [_]
          (st/emit! (dc/remove-all-strokes ids)))

        handle-detach
        (mf/use-callback
         (mf/deps ids)
         (fn [index]
           (fn [color]
             (let [remove-multiple (fn [[_ value]] (not= value :multiple))
                   current-stroke-color (-> (into {} (filter remove-multiple) current-stroke-color)
                                            (assoc :id nil :file-id nil))]
               ;; TODO CHECK
               (st/emit! (dc/change-stroke ids current-stroke-color 0))))))

        on-stroke-style-change
        (fn [index]
          (fn [event]
            (let [value (-> (dom/get-target event)
                            (dom/get-value)
                            (d/read-string))]
              (st/emit! (dc/merge-stroke ids {:stroke-style value} index)))))

        on-stroke-alignment-change
        (fn [index]
          (fn [event]
            (let [value (-> (dom/get-target event)
                            (dom/get-value)
                            (d/read-string))]
              (when-not (str/empty? value)
                (st/emit! (dc/merge-stroke ids {:stroke-alignment value} index))))))

        on-stroke-width-change
        (fn [index]
          (fn [value]
            (when-not (str/empty? value)
              (st/emit! (dc/merge-stroke ids {:stroke-width value} index)))))

        open-caps-select
        (fn [caps-state]
          (fn [event]
            (let [window-size (dom/get-window-size)

                  target (dom/get-current-target event)
                  rect   (dom/get-bounding-rect target)

                  top (if (< (+ (:bottom rect) 320) (:height window-size))
                        (+ (:bottom rect) 5)
                        (- (:height window-size) 325))

                  left (if (< (+ (:left rect) 200) (:width window-size))
                         (:left rect)
                         (- (:width window-size) 205))]
              (swap! caps-state assoc :open? true
                     :left left
                     :top top))))

        close-caps-select
        (fn [caps-state]
          (fn [_]
            (swap! caps-state assoc :open? false)))

        on-stroke-cap-start-change
        (fn [index value]
          (println "on-stroke-cap-start-change" index value)
          (st/emit! (dc/merge-stroke ids {:stroke-cap-start value} index)))

        on-stroke-cap-end-change
        (fn [index value]
          (println "on-stroke-cap-end-change" index value)
          (st/emit! (dc/merge-stroke ids {:stroke-cap-end value} index)))

        on-stroke-cap-switch
        (fn [index]
          (let [stroke-cap-start (get-in values [:strokes index :stroke-cap-start])
                stroke-cap-end   (get-in values [:strokes index :stroke-cap-end])]
            (when (and (not= stroke-cap-start :multiple)
                       (not= stroke-cap-end :multiple))
              (st/emit! (dc/merge-stroke ids {:stroke-cap-start stroke-cap-end
                                              :stroke-cap-end stroke-cap-start} index)))))
        on-add-stroke
        (fn [_]
          (st/emit! (dc/add-stroke ids {:stroke-style :solid
                                        :stroke-color clr/black
                                        :stroke-opacity 1
                                        :stroke-width 1})))
        _ (println "VALUES" values)]

    [:div.element-set
     [:div.element-set-title
      [:span label]
      [:div.add-page {:on-click on-add-stroke} i/close]]

     ;; TODO multiple
     [:div.element-set-content
      (cond
        (= :multiple (:strokes values))
        [:div.element-set-options-group
         [:div.element-set-label (tr "settings.multiple")]
         [:div.element-set-actions
          [:div.element-set-actions-button {:on-click handle-remove-remove-all}
           i/minus]]]


        (seq (:strokes values))
        (for [[index value] (d/enumerate (:strokes values []))]
          ;; Stroke Color
          [:*
           [:& color-row {:color {:color (:stroke-color value)
                                  :opacity (:stroke-opacity value)
                                  :id (:stroke-color-ref-id value)
                                  :file-id (:stroke-color-ref-file value)
                                  :gradient (:stroke-color-gradient value)}
                          :title (tr "workspace.options.stroke-color")
                          :on-change (handle-change-stroke-color index)
                          :on-detach (handle-detach index)
                          :on-remove (handle-remove index)}]

           ;; Stroke Width, Alignment & Style
           [:div.row-flex
            [:div.input-element
             {:class (dom/classnames :pixels (not= (:stroke-width value) :multiple))
              :title (tr "workspace.options.stroke-width")}

             [:> numeric-input
              {:min 0
               :value (-> (:stroke-width value) width->string)
               :precision 2
               :placeholder (tr "settings.multiple")
               :on-change (on-stroke-width-change index)}]]

            [:select#style.input-select {:value (enum->string (:stroke-alignment value))
                                         :on-change (on-stroke-alignment-change index)}
             (when (= (:stroke-alignment value) :multiple)
               [:option {:value ""} "--"])
             [:option {:value ":center"} (tr "workspace.options.stroke.center")]
             [:option {:value ":inner"} (tr "workspace.options.stroke.inner")]
             [:option {:value ":outer"} (tr "workspace.options.stroke.outer")]]

            [:select#style.input-select {:value (enum->string (:stroke-style value))
                                         :on-change (on-stroke-style-change index)}
             (when (= (:stroke-style value) :multiple)
               [:option {:value ""} "--"])
             [:option {:value ":solid"} (tr "workspace.options.stroke.solid")]
             [:option {:value ":dotted"} (tr "workspace.options.stroke.dotted")]
             [:option {:value ":dashed"} (tr "workspace.options.stroke.dashed")]
             [:option {:value ":mixed"} (tr "workspace.options.stroke.mixed")]]]

           ;; Stroke Caps
           (when show-caps
             [:div.row-flex
              [:div.cap-select {:tab-index 0 ;; tab-index to make the element focusable
                                :on-click (open-caps-select start-caps-state)}
               (value->name (:stroke-cap-start value))
               [:span.cap-select-button
                i/arrow-down]]
              [:& dropdown {:show (:open? @start-caps-state)
                            :on-close (close-caps-select start-caps-state)}
               [:ul.dropdown.cap-select-dropdown {:style {:top  (:top @start-caps-state)
                                                          :left (:left @start-caps-state)}}
                (for [[value label separator] (stroke-cap-names)]
                  (let [img (value->img value)]
                    [:li {:class (dom/classnames :separator separator)
                          :on-click #(on-stroke-cap-start-change index value)}
                     (when img [:img {:src (value->img value)}])
                     label]))]]

              [:div.element-set-actions-button {:on-click #(on-stroke-cap-switch index)}
               i/switch]

              [:div.cap-select {:tab-index 0
                                :on-click (open-caps-select end-caps-state)}
               (value->name (:stroke-cap-end value))
               [:span.cap-select-button
                i/arrow-down]]
              [:& dropdown {:show (:open? @end-caps-state)
                            :on-close (close-caps-select end-caps-state)}
               [:ul.dropdown.cap-select-dropdown {:style {:top  (:top @end-caps-state)
                                                          :left (:left @end-caps-state)}}
                (for [[value label separator] (stroke-cap-names)]
                  (let [img (value->img value)]
                    [:li {:class (dom/classnames :separator separator)
                          :on-click #(on-stroke-cap-end-change index value)}
                     (when img [:img {:src (value->img value)}])
                     label]))]]])]))]]))
