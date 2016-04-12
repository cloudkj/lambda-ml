(ns lambda-ml.util
  (:import [java.awt Color]
           [java.awt.image BufferedImage]))

(defn pixels->image
  [pixels]
  (let [w (count (first pixels))
        h (count pixels)]
    (reduce (fn [image [y row]]
              (reduce (fn [image [x pixel]]
                        (let [[r g b] (repeat 3 (int (* pixel 255)))]
                          (doto image
                            (.setRGB x y (.getRGB (Color. r g b))))))
                      image
                      (map-indexed vector row)))
            (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
            (map-indexed vector pixels))))
