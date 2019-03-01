(import [big_ol_pile_of_manim_imports [*]])
(import [mobject.types.image_mobject [*]])

(defclass CursorChar [VGroup]
  (defn __init__ [self ch]
    (.__init__ (super))
    (setv ch (cond
               [(= " " ch) "\\textunderscore"]
               [(= "{" ch) "["]
               [(= "}" ch) "]"]
               [True ch]))
    (.add self (SingleStringTexMobject (+ "\\texttt{" ch "}")))
    (setv self.char ch)
    (setv self.cursor (SurroundingRectangle self)))
  (defn show_cursor_animation [self]
    (Write self.cursor :run_time 0.5))
  (defn hide_cursor_animation [self]
    (FadeOut self.cursor :run_time 0.4)))

(defclass CursorString [VGroup]
  (defn __init__ [self ss &optional [cursor_index -1] [charspacing 1]]
    (setv
      self.cursor_index cursor_index
      self.charspacing charspacing
      self.string ss)
    (.__init__ (super)))
  (defn re-init [self ss]
    (setv
      prev None
      max_width 0
      max_height 0)
    (for [s ss]
      (print "s=" s)
      (setv cc (CursorChar s))
      (.add self cc)
      (when prev
        (.next_to cc prev (* self.charspacing RIGHT))
        (.align_to cc prev DOWN))
      (setv
        prev cc
        self.max_width (max max_width (.get_width cc))
        self.max_height (max max_height (.get_height cc)))))
  (defn generate_points [self]
    (.re-init self self.string))
  (defn cursor_rect [self]
    (setv
      max_width 0
      max_height 0)
    (for [o self.submobjects]
      (setv
        self.max_width (max max_width (.get_width o))
        self.max_height (max max_height (.get_height o))))
    (print "cursor_rect" (len self.submobjects) "objects " "index=" self.cursor_index)
    (doto (Rectangle :width (+ self.max_width (* 2 SMALL_BUFF))
                     :height (+ self.max_height (* 2 SMALL_BUFF)))
          (.move_to (get self.submobjects self.cursor_index))))
  (defn show_cursor [self]
    (.add self (.cursor_rect self))
    self))

(defclass TextDiffPosition [VGroup]
  (defn __init__ [self astring ai bstring bi &optional [stroke 4]]
    (setv
      self.a (CursorString astring ai)
      self.b (CursorString bstring bi))
    (doto (super)
          (.__init__ (doto self.a (.add (doto (.cursor_rect self.a) (.set_stroke :width stroke))))
                     (doto self.b (.add (doto (.cursor_rect self.b) (.set_stroke :width stroke)))))
          (.arrange_submobjects DOWN)))
  
  (defn write-animation [self]
    (AnimationGroup #* (lfor obj self.submobjects (Write obj)))))

(defclass DiffGrid [VMobject]
  (defn __init__ [self hstring vstring]
    (setv self.hstring hstring
          self.vstring vstring
          self.columns (len hstring)
          self.rows (len vstring)
          ;; parts
          self.grid_lines (VGroup)
          self.straight_arrows (VGroup)
          self.diagonal_arrows (VGroup)
          ;; self.diffpositions (VGroup)
          self.labels (VGroup)
          self.bubbles (VGroup)
          self.pos_to_bubble (lfor i (range (len hstring))
                                   (lfor j (range (len vstring)) None)))
    (.__init__ (super) :width (len hstring) :height (len vstring))
    (.add self
          self.labels
          self.grid_lines
          #_self.diffpositions
          self.bubbles
          self.straight_arrows
          self.diagonal_arrows))
  (defn top_left_corner [self]
    [(- 0 (/ self.width 2.0))
     (/ self.height 2.0)
     0])
  (defn top_left [self]
    (np.add (.top_left_corner self)
            [(/ self.width self.columns 2.0)
             (/ self.height self.rows 2.0)
             0]))
  (defn right [self i]
    [(* i (/ self.width self.columns)) 0 0])
  (defn down [self i]
    [0 (* i (- 0 (/ self.height self.rows))) 0])
  (defn grid_position [self x y]
    [(+ (- 0 (/ self.width 2.0))
        (/ self.width self.columns 2.0)
        (* x (/ self.width self.columns)))
     (+ (- (/ self.height 2.0) (/ self.height self.rows 2.0))
        (* y (- 0 (/ self.height self.rows))))
     0])
  (defn generate_points [self]
    (setv
      w self.width
      h self.height
      grid_lines (VGroup))
    (for [i (range (inc (len self.hstring)))]
      (setv top (np.add (.top_left_corner self) (.right self i)))
      (.add self.grid_lines (Line top (np.add top (.down self (len self.vstring)))))
      (unless (zero? i)
        (.add self.labels (doto (TextMobject (get self.hstring (dec i)) :color RED)
                                (.next_to top UP)
                                (.shift (.right self -0.5))))))

    (for [i (range (inc (len self.vstring)))]
      (setv left (np.add (.top_left_corner self) (.down self i)))
      (.add self.grid_lines (Line left (np.add left (.right self (len self.hstring)))))
      (unless (zero? i)
        (.add self.labels (doto (TextMobject (get self.vstring (dec i)) :color RED)
                                (.next_to left LEFT)
                                (.shift (.down self -0.5))))))

    (for [x (range (len self.hstring))
          y (range (len self.vstring))]
      ;; (setv stroke (/ (max self.rows self.columns) 8))
      ;; (setv text (TextDiffPosition self.hstring x self.vstring y stroke))
      ;; (.move_to text (.grid_position self x y))
      ;; (.set_width text (- (/ self.width self.columns) (* 3 SMALL_BUFF)))
      ;; (.add self.diffpositions text)
      ;; (.add self.diffpositions (doto (SurroundingRectangle text)
      ;;                                (.set_stroke :width (/ (max self.rows self.columns) 3))))
      (setv bubble (doto (Circle :color WHITE)
                         (.set_width (- (/ self.width self.columns) (* 2 SMALL_BUFF)))
                         (.move_to (.grid_position self x y))))
      (.add self.bubbles bubble)
      (setv (get self.pos_to_bubble x y) bubble)

      (unless (= x (dec (len self.hstring)))
        (.add self.straight_arrows
              (doto (Arrow (.grid_position self x y)
                           (.grid_position self (inc x) y)
                           :color BLUE)
                    (.scale 1))))
      (unless (= y (dec (len self.vstring)))
        (.add self.straight_arrows
              (doto (Arrow (.grid_position self x y)
                           (.grid_position self x (inc y))
                           :color BLUE)
                    (.scale 1))))

      (when (and (< x (dec (len self.hstring)))
                 (< y (dec (len self.vstring)))
                 (= (get self.hstring x) (get self.vstring y)))
        (.add self.diagonal_arrows
              (doto (Arrow (.grid_position self x y)
                           (.grid_position self (inc x) (inc y))
                           :color GREEN)
                    (.scale 0.7)))))
    (setv self.start_square (doto (Rectangle :width (/ self.width self.columns)
                                             :height (/ self.height self.columns)
                                             :color GREEN
                                             :fill_opacity 0.8)
                                  (.move_to (.grid_position self 0 0))))
    (.add self self.start_square)))

(defclass TextDiffScene [Scene]
  [CONFIG {"name" "myscene"}]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/04-text-diff.mp4")
  (defn construct [self]
    (setv
      self.astring "LADDER"
      self.bstring "LEADER"
      self.aindex 0
      self.bindex 0
      self.show_dot False
      self.cost 0
      self.cs (doto (CursorString self.astring 0)
                    (.set_width (- (/ FRAME_WIDTH 2) LARGE_BUFF))
                    (.center))
      self.cursor (.cursor_rect self.cs)
      
      self.diffgrid (doto (DiffGrid self.astring self.bstring)
                          (.set_width (- FRAME_WIDTH SMALL_BUFF))
                          (.set_height (- FRAME_HEIGHT LARGE_BUFF)))
      self.cost-obj (Integer self.cost)
      cost-group (doto (VGroup (TextMobject "cost") self.cost-obj )
                       (.arrange_submobjects RIGHT))
      cs-and-cost-group (doto
                          (VGroup (VGroup self.cs self.cursor) cost-group)
                          (.arrange_submobjects DOWN))
      self.prev_dots [])

    (doto self
          (.play
            (ShowCreation self.cs)
            (ShowCreation self.cursor)
            (ShowCreation cost-group))
          (.wait 5)
          (.play-edit-script [:advance :delete [:insert "E"] :delete [:insert "A"] :advance :advance]))
    
    (.reset self)
    
    (setv
      grid-surround (SurroundingRectangle self.diffgrid :color BLACK)
      grid-group (VGroup grid-surround))
    
    (setv top-group (VGroup grid-surround
                            (doto
                              (VGroup (VGroup self.cs self.cursor) cost-group)
                              (.arrange_submobjects DOWN))))
    (doto self
          (.play (ShowCreation grid-surround) :run_time 0.1)
          (.play (ApplyMethod (. top-group arrange_submobjects) RIGHT)))
    (.move_to self.diffgrid grid-surround)
    (setv self.show_dot True)
    (doto self
          (.create-dot)
          (.play (Write self.diffgrid.labels)
                 (ShowCreation self.diffgrid.grid_lines))
          (.play (Write self.dot))
          (.wait 5)
          (.play-edit-script [:advance :delete [:insert "E"] :delete [:insert "A"] :advance :advance])
          (.wait 5))
    
    (.reset self) 
    (.play-edit-script self [:advance :delete :delete :delete :delete [:insert "E"] [:insert "A"] [:insert "D"] [:insert "E"]])
    (.wait self 5)
    
    (.reset self)
    (.play-edit-script self [:advance :delete :delete [:insert "E"] [:insert "A"] :advance :advance])
    (.wait self 5)
    (doto self
          (.play (FadeOut self.diffgrid.grid_lines))
          (.play (ShowCreation self.diffgrid.bubbles))
          (.play (ShowCreation self.diffgrid.straight_arrows))
          (.play (ShowCreation self.diffgrid.diagonal_arrows))
          (.wait 5)
          (.clear-dots)
          (.wait 5)))
  
  (defn create-dot [self]
    (setv self.dot (doto (Circle :radius 0.4 :color WHITE :fill_opacity 1)
                         (.move_to (.grid_position self 0 0)))))
  (defn reset [self]
    (setv
      prevcs self.cs
      self.cs (doto (CursorString self.astring 0)
                    (.replace prevcs))
      prevcursor self.cursor
      self.cursor (.cursor_rect self.cs)
      self.aindex 0
      self.bindex 0
      self.cost 0)
    (.play self
           (FadeOut prevcs)
           (FadeOut prevcursor))
    (.play self
           (Write self.cs)
           (ShowCreation self.cursor)
           (ChangeDecimalToValue self.cost-obj self.cost :run_time 0.4))
    (when self.show_dot
      (doto self
            (.clear-dots)
            (.create-dot)
            (.play (Write self.dot)))))

  (defn play-edit-script [self es]
    (for [e es]
      (cond
        [(= e :advance) (.play-advance-cursor self self.cs)]
        [(= e :delete) (.play-delete-char self self.cs)]
        [(= (get e 0) :insert) (.play-insert-char self self.cs (get e 1))])))
  
  (defn grid-position [self i j]
    (.get_center (get self.diffgrid.pos_to_bubble i j)))

  (defn move-dot-animation [self i j]
    (if-not self.show_dot
            (EmptyAnimation)
            (do (setv newdot (.copy self.dot)
                      anim (ApplyMethod (. newdot move_to) (.grid-position self i j)))
                (.append self.prev_dots self.dot)
                (setv self.dot newdot)
                anim)))
  
  (defn clear-dots [self]
    (.play self
           (FadeOut self.dot)
           #* (lfor d self.prev_dots (FadeOut d)))
    (setv self.prev_dots []))
  
  (defn play-delete-char [self cs]
    (setv
      self.aindex (inc self.aindex)
      self.cost (inc self.cost))
    (.play self
           (FadeOut (get cs.submobjects cs.cursor_index))
           (.move-dot-animation self self.aindex self.bindex)
           (ChangeDecimalToValue self.cost-obj self.cost)
           #* (lfor idx (range (inc cs.cursor_index) (len cs.submobjects))
                    (ApplyMethod (. (get cs.submobjects idx) shift)
                                 [(get (- (.get_center (get cs.submobjects (dec idx)))
                                          (.get_center (get cs.submobjects idx)))
                                       0) 0 0])))
    
    (.remove cs (get cs.submobjects cs.cursor_index)))

  (defn play-insert-char [self cs c]
    (setv
      crect (.cursor_rect cs)
      oldchar (get cs.submobjects cs.cursor_index)
      newchar (doto (CursorChar c)
                    (.move_to crect)
                    (.set_height (. cs max_height))
                    (.align_to oldchar DOWN))
      dx (get (if (= (inc cs.cursor_index) (len cs.submobjects))
                  (- (.get_center self.cursor)
                     (.get_center (get cs.submobjects (dec cs.cursor_index))))
                  (- (.get_center (get cs.submobjects (inc cs.cursor_index)))
                     (.get_center self.cursor)))
              0)
      self.bindex (inc self.bindex)
      self.cost (inc self.cost))
    (.play self
           (Write newchar)
           (ApplyMethod (. self.cursor shift) [dx 0 0])
           (ChangeDecimalToValue self.cost-obj self.cost)
           (.move-dot-animation self self.aindex self.bindex)
           #* (lfor idx (range cs.cursor_index (len cs.submobjects))
                    (ApplyMethod (. (get cs.submobjects idx) shift) [dx 0 0])))
    (.insert cs.submobjects cs.cursor_index newchar)
    (setv cs.cursor_index (inc cs.cursor_index)))
  
  (defn play-advance-cursor [self cs]
    (setv
      self.aindex (inc self.aindex)
      self.bindex (inc self.bindex))
    (.play self
           (.move-dot-animation self self.aindex self.bindex)
           (ApplyMethod (. self.cursor shift)
                        [(get (if (= (inc cs.cursor_index) (len cs.submobjects))
                                  (- (.get_center self.cursor)
                                     (.get_center (get cs.submobjects (dec cs.cursor_index))))
                                  (- (.get_center (get cs.submobjects (inc cs.cursor_index)))
                                     (.get_center self.cursor)))
                              0)
                         0 0]))
    (setv cs.cursor_index (inc cs.cursor_index))))

(defclass GithubDiffScene [Scene]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/02-github-diff.mp4")
  (defn construct [self]
    (setv img (doto (ImageMobject "github-diff.png")
                    (.set_width (- FRAME_WIDTH 1))))
    (.play self (FadeInFromLarge img))
    (.wait self 5)
    (.play self (FadeOut img))))

(defclass AutochromeDiffScene [Scene]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/02-autochrome-diff.mp4")
  (defn construct [self]
    (setv img (doto (ImageMobject "autochrome-diff.png")
                    (.set_width (- FRAME_WIDTH 1))))
    (.play self (SpinInFromNothing img))
    (.wait self 5)
    (.play self (FadeOut img))))

(defclass TitleScene [Scene]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/01-title.mp4")
  (defn construct [self]
    (setv
      title (doto (Title "Discovering an Algorithm for Structural Diffs"))
      bullets (BulletedList "Why do we care?"
                            "What is a diff?"
                            "Text diffs"
                            "Diffing as pathfinding"
                            "Structural diffs")
      grp (doto (VGroup title bullets)
                (.set_width (- FRAME_WIDTH SMALL_BUFF))
                (.arrange_submobjects DOWN)))
    (doto self (.play (Write title)))
    (for [b bullets.submobjects]
      (.play self (Write b)))
    (.wait self 5)
    (.play self (FadeOut grp))))

(defn indented-line [s]
  (print "indented-line" s)
  (setv
    charspacing 0.2
    noindent (.lstrip s)
    indent-len (- (len s) (len noindent))
    cstring (CursorString noindent -1 charspacing)
    ;;indent-spacer (Rectangle :height (.get_height cstring) :width (* indent-len cstring.max_width))
    indent-spacer (CursorString (* " " indent-len) -1 charspacing)
    grp (VGroup indent-spacer cstring))
  
  (.align_to indent_spacer cstring DOWN)
  (doto grp
        (.arrange_submobjects RIGHT)
        (.add (SurroundingRectangle grp))))

(defn lines-group [s]
  (setv group (VGroup #* (lfor line (.split s "\n")
                               (indented-line line))))
  (.arrange_submobjects group DOWN)
  (for [obj (cut group.submobjects 1)]
    (.align_to obj (get group.submobjects 0) LEFT))
  group)

(defn descender?
  [ch]
  (or (= ch "q") (= ch "j") (= ch "p") (= ch "y") (= ch "g")))

(defclass StretchyEllipse [Ellipse]
  (defn replace-stretch [self obj]
    (.replace self obj :stretch True)))

(defclass StretchyRoundedRect [RoundedRectangle]
  (defn replace-stretch [self obj]
    (.replace self obj :stretch True)))

(defclass StretchyRect [Rectangle]
  (defn replace-stretch [self obj]
    (.replace self obj :stretch True)))

(defn closer
  [ch]
  (cond
    [(= ch "[") "]"]
    [(= ch "(") ")"]
    [(= ch "{") "}"]))

(defn end-of-thing
  [s idx]
  (setv the-closer (closer (get s idx)))
  (cond
    [the-closer
     (setv stack [the-closer])
     (for [i (range (inc idx) (len s))]
       (setv icloser (closer (get s i)))
       (cond
         [(= (get s i) (get stack -1))
          (.pop stack)
          (when (empty? stack)
            (return (inc i)))]
         [icloser
          (.append stack icloser)]))]
    [True
     (for [i (range (inc idx) (len s))]
       (when (and (or (= " " (get s i))
                      (= "\n" (get s i))
                      (= "]" (get s i))
                      (= ")" (get s i))
                      (= "}" (get s i))))
         (return i)))
     (return -1)]))

(defn start-of-next-thing
  [s idx]
  (if (closer (get s idx))
      (inc idx)
      (do (for [i (range (end-of-thing s idx) (len s))]
            (unless (or (= " " (get s i))
                        (= "\n" (get s i))
                        (= "]" (get s i))
                        (= ")" (get s i))
                        (= "}" (get s i)))
              (return i)))
          (return -1))))

(defn iterate-things
  [s]
  (setv i 0)
  (while (not (neg? i))
    (setv end (end-of-thing s i))
    (print i end)
    (print (cut s i end))
    (print "")
    (setv i (start-of-next-thing s i))))

(defclass CodeScene [Scene]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/code.mp4")
  (defn construct [self]
    (setv
      mystring "(defn example\n  [x]\n  (println \"hello!\")\n  {:more (inc x)\n   :less (dec x)}"
      lines (doto (SexprGroup mystring PURPLE)
                  (.set_height (- FRAME_WIDTH (* 2 LARGE_BUFF)))
                  (.set_width (- FRAME_WIDTH (* 2 LARGE_BUFF)))
                  (.scale 0.5)
                  (.center)))
    (.play self (Write lines))
    (setv cursor (.index-box lines 0 (len mystring)))
    (.wait self 5)))

(defclass SexprGroup [VGroup]
  (defn __init__ [self text cursor-color]
    (.__init__ (super))
    (setv
      self.text text
      self.cursor-color cursor-color
      position [0 0 0]
      width 0
      height 0
      char-objs []
      items [])
    (for [ch text]
      (.append char-objs
               (cond
                 [(= ch " ") :space]
                 [(= ch "\n") :newline]
                 [True
                  (setv
                    obj (CursorChar ch)
                    width (max width (.get_width obj))
                    height (max height (.get_height obj)))

                  obj])))
    (setv self.width width
          self.height height)
    (for [c char-objs]
      (cond
        [(= c :space)
         (setv position (np.add position [width 0 0]))
         (.append items None)]
        [(= c :newline)
         (setv position [0 (- (get position 1) (* 1.5 height)) 0])
         (.append items None)]
        [True
         (.shift c (- position (.get_critical_point c DOWN)))
         (when (descender? c.char)
           (.shift c [0 (- 0 (* 0.1 np.e height)) 0]))
         (when (= c.char "\"")
           (.shift c [0 (+ 0 (* 0.5 height)) 0]))
         (when (= c.char "-")
           (.shift c [0 (+ 0 (* 0.25 height)) 0]))
         
         (.add self c)
         (.append items c)
         (setv position (np.add position [width 0 0]))]))
    (setv self.items items))
  
  (defn sexpr-box [self i]
    (setv end (inc (end-of-thing self.text i)))
    (print "sexpr box " (cut self.text i end))
    (.index-box self i end))

  (defn index-box [self i j]
    (setv enclosing-group (VGroup #* (lfor box (cut self.items i j) :if box box)))
    (print "index-box" i j "string=" (cut self.text i j))
    (doto (StretchyRect :color self.cursor-color :fill_opacity 0.5)
          (.replace-stretch enclosing-group)
          (.set_height (max self.height (.get_height enclosing-group)) :stretch True)))
  (defn char-box [self i color]
    (doto (StretchyRect :color color :fill_opacity 0.5)
          (.replace-stretch (get self.items i))
          (.set_height (max self.height (.get_height (get self.items i))) :stretch True)))
  (defn substring-box [self substr]
    (setv indexes (list (lfor i (range (len self.text))
                              :if (.startswith self.text substr i)
                              i)))
    (.index-box self (get indexes 0) (+ (len substr) (get indexes 0)))))

(defclass StructuralDiffScene [Scene]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/05-structural.mp4")
  (defn construct [self]
    (setv
      self.astring "(defn example\n  [x]\n  (println \"hello!\")\n  {:more (inc x)\n   :less (dec x)}"
      self.bstring "(defn example\n  [x]\n  (-> {:more (inc x)\n       :less (dec x)}\n      (conj :twice (+ x x))))"
      self.asexpr (doto (SexprGroup self.astring BLUE)
                        (.set_width (/ FRAME_WIDTH 4)))
      self.bsexpr (doto (SexprGroup self.bstring PURPLE)
                        (.set_width (/ FRAME_WIDTH 4))
                        (.set_height (.get_height self.asexpr)))
      atitle (doto (TextMobject "Source")
                   (.next_to self.asexpr (* TOP 0.2)))
      agroup (VGroup self.asexpr atitle)
      btitle (doto (TextMobject "Target")
                   (.next_to self.bsexpr (* TOP 0.2)))
      bgroup (VGroup self.bsexpr btitle)

      group (doto (VGroup (doto agroup (.add (SurroundingRectangle agroup)))
                          (doto bgroup (.add (SurroundingRectangle bgroup))))
                  (.set_width (/ FRAME_WIDTH 2))
                  (.arrange_submobjects RIGHT)
                  (.center))
      self.aindex 0
      self.bindex 0
      self.last-aindex 0
      self.last-aend 0
      self.last-bindex 0
      self.last-bend 0

      self.acursor (.index-box self.asexpr 0 (len self.astring))
      self.bcursor (.index-box self.bsexpr 0 (len self.bstring)))

    (.play self (Write group))

    (.play self
           (FadeIn self.acursor)
           (FadeIn self.bcursor))

    ;; prime it?
    (doto self (.advance-a-anim) (.advance-b-anim))
    
    (.wait self 5)
    (doto self
          (.advance-both)
          (.advance-both)
          (.advance-both)
          (.advance-both)
          (.advance-both))

    (.wait self 5)

    (.go-into-b-only self)
    (.wait self 5)

    (.advance-b self)
    (.wait self 5)

    (.advance-a-whole self)
    (.wait self 5)

    (.advance-both self)
    (.advance-both self)
    (.advance-both self)
    (.advance-both self)
    (.advance-both self)
    (.advance-both self)
    (.advance-both self)
    (.advance-both self)
    (.play self (.advance-b-anim self))
    (.wait self 5)
    (.play self (.fade-b-anim self GREEN))
    (.wait self 5)
    (.play self (FadeOut self.acursor))
    (.wait self 10))
  
  (defn go-into-b-only [self]
    (setv
      mark-left (Write (.char-box self.bsexpr self.last-bindex GREEN))
      mark-right (Write (.char-box self.bsexpr (dec self.last-bend) GREEN))
      advance (.advance-b-anim self))
    (.play self mark-left mark-right advance))
  
  (defn advance-a-anim [self]
    (setv
      end (end-of-thing self.astring self.aindex)
      anim (ApplyMethod (. self.acursor replace-stretch) (.index-box self.asexpr self.aindex end))
      self.last-aindex self.aindex
      self.last-aend end
      self.aindex (start-of-next-thing self.astring self.aindex))
    anim)

  (defn advance-a-whole-anim [self]
    ;; QQ
    (setv
      left-paren-pos self.last-aindex
      right-paren-pos (dec self.last-aend)
      start (start-of-next-thing self.astring self.last-aend)
      end (end-of-thing self.astring start)
      anim (ApplyMethod (. self.acursor replace-stretch) (.index-box self.asexpr start end))
      self.last-aindex self.aindex
      self.last-aend end
      self.aindex (start-of-next-thing self.astring start))
    anim)

  (defn advance-b-whole-anim [self]
    ;; QQ
    (setv
      left-paren-pos self.last-bindex
      right-paren-pos (dec self.last-bend)
      start (start-of-next-thing self.bstring self.last-bend)
      end (end-of-thing self.bstring start)
      anim (ApplyMethod (. self.bcursor replace-stretch) (.index-box self.bsexpr start end))
      self.last-bindex self.bindex
      self.last-bend end
      self.bindex (start-of-next-thing self.bstring start))
    anim)

  (defn advance-b-anim [self]
    (setv
      end (end-of-thing self.bstring self.bindex)
      anim (ApplyMethod (. self.bcursor replace-stretch) (.index-box self.bsexpr self.bindex end))
      self.last-bindex self.bindex
      self.last-bend end
      self.bindex (start-of-next-thing self.bstring self.bindex))
    anim)

  (defn advance-a [self]
    (setv
      fade (.fade-a-anim self RED)
      adv (.advance-a-anim self))
    (.play self fade adv))

  (defn advance-a-whole [self]
    (setv
      fade (.fade-a-anim self RED)
      adv (.advance-a-whole-anim self))
    (.play self fade adv))

  (defn advance-b-whole [self]
    (setv
      fade (.fade-b-anim self RED)
      adv (.advance-b-whole-anim self))
    (.play self fade adv))

  (defn advance-b [self]
    (setv
      fade (.fade-b-anim self GREEN)
      adv (.advance-b-anim self))
    (.play self fade adv))

  (defn advance-both [self]
    (setv aa (.advance-a-anim self)
          ba (.advance-b-anim self))
    (.play self aa ba))

  (defn fade-a-anim [self color]
    (setv newcursor (.copy self.acursor))
    (setv anim (FadeToColor self.acursor color))
    (setv self.acursor newcursor)
    anim)

  (defn fade-b-anim [self color]
    (setv newcursor (.copy self.bcursor))
    (setv anim (FadeToColor self.bcursor color))
    (setv self.bcursor newcursor)
    anim))

(defclass TextDiffIntro [Scene]
  (defn get_movie_file_path [self &optional name ext] "/tmp/manim/03-text-diff-intro.mp4")
  (defn construct [self]
    (setv
      astring "LADDER"
      bstring "LEADER"
      acs (doto (CursorString astring)
                (.set_width (- (/ FRAME_WIDTH 2) LARGE_BUFF))
                (.center)
                (.shift (/ UP 2)))
      bcs (doto (CursorString bstring)
                (.set_width (- (/ FRAME_WIDTH 2) LARGE_BUFF))
                (.next_to acs DOWN)))
    (doto self
          (.play (Write acs))
          (.wait 2)
          (.play (FadeInFrom bcs UP))
          (.play
            (FadeToColor (get bcs.submobjects 1) YELLOW :run_time 1)
            (FadeToColor (get bcs.submobjects 2) YELLOW :run_time 1))
          (.wait 5)
          (.play (FadeOut acs)
                 (FadeOut bcs)))))

(setv config
      {"camera_config" {"pixel_width" 3840
                        "pixel_height" 2160}
       "frame_duration" (/ 1 60)
       "skip_animations" False
       "write_to_movie" True
       "save_pngs" False
       "movie_file_extension" ".mp4"})

#_(GithubDiffScene #** config)
#_(TextDiffIntro #** config)
#_(TitleScene #** config)
(AutochromeDiffScene #** config)

#_(do
  #_(TitleScene #** config)
  #_(GithubDiffScene #** config)
  #_(CodeScene #** config)
  #_(TextDiffIntro #** config)
  (TextDiffScene #** config)
  #_(StructuralDiffScene #** config))!
