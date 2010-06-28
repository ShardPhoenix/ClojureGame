(ns org.au.game
  (:import
    (java.awt Color Dimension)
    (java.awt.event KeyListener)
    (javax.swing JFrame JOptionPane JPanel))
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE VK_SHIFT)

(def max-fps 30)
(def min-millis-per-frame (long (/ 1000 max-fps)))
(def window-width 800)
(def window-height 600)

(def player
  {:x 400
   :y 450
   :height 13
   :width 11
   :x-speed 5
   :y-speed 5
   :hp 100
   :shield 100
   :frames-per-bullet 3
   :damager-per-bullet 10})

(def initial-gamestate {:player player
                        :playerbullets []
                        :enemies []})

(defstruct playerbullet :x :y :speed :width :height)
(defn create-playerbullet [x y]
  (struct playerbullet x y -10 7 7))

(defstruct enemy 
  :type 
  :x 
  :y 
  :speed 
  :width 
  :height 
  :frames-per-bullet 
  :hp)
(defn create-standard-enemy [x y]
    (struct enemy :standard x y 5 8 8 5 20))

(def images nil) ;map from item-image-type to render function? - need to render in right order too

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn render-playerbullets [gfx bullets]
	(doseq [bullet bullets]
	 (.setColor gfx (color :red))
	 (.fillOval gfx (bullet :x) (bullet :y) (bullet :width) (bullet :height))))

(defn render-player [gfx player]
  (.setColor gfx (color :blue))
  (.fillRect gfx (player :x) (player :y) (player :width) (player :height)))

(defn render-background [gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 window-width window-height))

(defn render-enemies [gfx enemies]
  (doseq [enemy enemies]
    (.setColor gfx (color :black))
    (.fillRect gfx (:x enemy) (:y enemy) (:width enemy) (:height enemy))))

(defn render [game window]
  (let [gfx (.getGraphics window)
        player (:player game)
        playerbullets (:playerbullets game)
        enemies (:enemies game)]
      (render-background gfx)
      (render-player gfx player)
      (render-playerbullets gfx playerbullets)
      (render-enemies gfx enemies)))

(defn update-player-bullets [playerbullets]
  (map #(assoc % :y (+ (:y %) (:speed %))) playerbullets))

(defn update-player [player input ]
  (let [{:keys [x y x-speed y-speed]} player
        {:keys [x-direc y-direc fire?]} input
        new-x (+ x (* x-speed x-direc))
        new-y (+ y (* y-speed y-direc))
        new-x-ok? (and (>= new-x 0) (<= new-x window-width))
        new-y-ok? (and (>= new-y 0) (<= new-y window-height))]
		  (assoc player 
			  :x (if new-x-ok? new-x x)
			  :y (if new-y-ok? new-y y))))

(defn update-enemies [enemies]
  (if (empty? enemies)
    (conj enemies (create-standard-enemy 200 200))
     enemies))

(defn update [input game frame]
  (println frame)
  (let [player (:player game)
        enemies (update-enemies (:enemies game))
        {:keys [x y]} player
        {:keys [fire?]} input
        playerbullets (update-player-bullets (:playerbullets game))
        should-fire? (and fire? (zero? (rem frame (:frames-per-bullet player))))]
    (assoc game :player (update-player player input)
                :playerbullets (if should-fire?
                                 (conj playerbullets (create-playerbullet x y))
                                  playerbullets)
                :enemies enemies)))

(defn get-input [keys-set] 
  (let [left (if (keys-set VK_LEFT) -1 0)
        right (if (keys-set VK_RIGHT) 1 0)
        up (if (keys-set VK_UP) -1 0)
        down (if (keys-set VK_DOWN) 1 0)]
        {:x-direc (+ left right) 
         :y-direc (+ up down) 
         :fire? (or (keys-set VK_SPACE) (keys-set VK_SHIFT))}))

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(defn create-panel [width height key-code-atom]
  (proxy [JPanel KeyListener]
    [] ; superclass constructor arguments
    (getPreferredSize [] (Dimension. width height))
    (keyPressed [e]
      (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom (.getKeyCode e))))
    (keyReleased [e]
      (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom (.getKeyCode e))))
    (keyTyped [e]) ; do nothing
    ))

(defn configure-gui [window panel]
  (doto panel
    (.setFocusable true) ; won't generate key events without this
    (.addKeyListener panel))
  (doto window
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)))

(let [window (JFrame. "Game")
      keys-set-atom (atom #{}) ;set of keyboard keys currently being held down by player
      panel (create-panel window-width window-height keys-set-atom)]
  (configure-gui window panel)
	(loop [gamestate initial-gamestate
	       frame 1]
	  (let [input (get-input @keys-set-atom)
	        start-time (current-time)]
	  (let [updated-gamestate (update input gamestate frame)]
	       (render gamestate window)
	  (let [render-time (- (current-time) start-time)
	        wait-time (max (- min-millis-per-frame render-time) 0)]
	    (java.lang.Thread/sleep wait-time))
	  (recur updated-gamestate (inc frame))))))
