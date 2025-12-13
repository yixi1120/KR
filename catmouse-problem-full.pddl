(define (problem catmouse-full-map)
  (:domain catmouse)

  ;; ----- Objects -----
  (:objects
    ;; Bedroom cells: (1..3 x 1..3)
    c1_1 c2_1 c3_1
    c1_2 c2_2 c3_2
    c1_3 c2_3 c3_3

    ;; Kitchen cells: (4..5 x 1..3)
    c4_1 c5_1
    c4_2 c5_2
    c4_3 c5_3

    ;; Door cells
    c3_4 c5_4

    ;; Living-room cells: (1..5 x 5..8)
    c1_5 c2_5 c3_5 c4_5 c5_5
    c1_6 c2_6 c3_6 c4_6 c5_6
    c1_7 c2_7 c3_7 c4_7 c5_7
    c1_8 c2_8 c3_8 c4_8 c5_8

    ;; Doors and keys
    bedroom_door kitchen_door - door
    bedroom_key  kitchen_key  - key
  )

  ;; ----- Initial state -----
  (:init
    ;; --- Cat and mouse initial positions ---
    ;; Cat starts at (1,1) in the bedroom.
    (cat-at c1_1)
    ;; Mouse is at (1,8) in the living room (mouse hole).
    (mouse-at c1_8)

    ;; --- Key-door pairing ---
    (key-for bedroom_key bedroom_door)
    (key-for kitchen_key kitchen_door)

    ;; --- Key positions (example; you can overwrite these per run) ---
    ;; Bedroom key inside the bedroom, not at the cat start.
    (key-at bedroom_key c2_2)
    ;; Kitchen key somewhere in the living room.
    (key-at kitchen_key c4_6)

    ;; --- Adjacency inside bedroom (1..3 x 1..3) ---

    ;; Horizontal edges in bedroom
    (adj c1_1 c2_1) (adj c2_1 c1_1)
    (adj c2_1 c3_1) (adj c3_1 c2_1)

    (adj c1_2 c2_2) (adj c2_2 c1_2)
    (adj c2_2 c3_2) (adj c3_2 c2_2)

    (adj c1_3 c2_3) (adj c2_3 c1_3)
    (adj c2_3 c3_3) (adj c3_3 c2_3)

    ;; Vertical edges in bedroom
    (adj c1_1 c1_2) (adj c1_2 c1_1)
    (adj c1_2 c1_3) (adj c1_3 c1_2)

    (adj c2_1 c2_2) (adj c2_2 c2_1)
    (adj c2_2 c2_3) (adj c2_3 c2_2)

    (adj c3_1 c3_2) (adj c3_2 c3_1)
    (adj c3_2 c3_3) (adj c3_3 c3_2)

    ;; Bedroom -> bedroom door cell (3,3) <-> (3,4)
    (adj c3_3 c3_4) (adj c3_4 c3_3)

    ;; --- Adjacency inside kitchen (4..5 x 1..3) ---

    ;; Horizontal edges in kitchen
    (adj c4_1 c5_1) (adj c5_1 c4_1)
    (adj c4_2 c5_2) (adj c5_2 c4_2)
    (adj c4_3 c5_3) (adj c5_3 c4_3)

    ;; Vertical edges in kitchen
    (adj c4_1 c4_2) (adj c4_2 c4_1)
    (adj c4_2 c4_3) (adj c4_3 c4_2)

    (adj c5_1 c5_2) (adj c5_2 c5_1)
    (adj c5_2 c5_3) (adj c5_3 c5_2)

    ;; Kitchen -> kitchen door cell (5,3) <-> (5,4)
    (adj c5_3 c5_4) (adj c5_4 c5_3)

    ;; --- Adjacency inside living room (1..5 x 5..8) ---

    ;; Horizontal edges, row y=5
    (adj c1_5 c2_5) (adj c2_5 c1_5)
    (adj c2_5 c3_5) (adj c3_5 c2_5)
    (adj c3_5 c4_5) (adj c4_5 c3_5)
    (adj c4_5 c5_5) (adj c5_5 c4_5)

    ;; row y=6
    (adj c1_6 c2_6) (adj c2_6 c1_6)
    (adj c2_6 c3_6) (adj c3_6 c2_6)
    (adj c3_6 c4_6) (adj c4_6 c3_6)
    (adj c4_6 c5_6) (adj c5_6 c4_6)

    ;; row y=7
    (adj c1_7 c2_7) (adj c2_7 c1_7)
    (adj c2_7 c3_7) (adj c3_7 c2_7)
    (adj c3_7 c4_7) (adj c4_7 c3_7)
    (adj c4_7 c5_7) (adj c5_7 c4_7)

    ;; row y=8
    (adj c1_8 c2_8) (adj c2_8 c1_8)
    (adj c2_8 c3_8) (adj c3_8 c2_8)
    (adj c3_8 c4_8) (adj c4_8 c3_8)
    (adj c4_8 c5_8) (adj c5_8 c4_8)

    ;; Vertical edges between y=5 and y=6
    (adj c1_5 c1_6) (adj c1_6 c1_5)
    (adj c2_5 c2_6) (adj c2_6 c2_5)
    (adj c3_5 c3_6) (adj c3_6 c3_5)
    (adj c4_5 c4_6) (adj c4_6 c4_5)
    (adj c5_5 c5_6) (adj c5_6 c5_5)

    ;; between y=6 and y=7
    (adj c1_6 c1_7) (adj c1_7 c1_6)
    (adj c2_6 c2_7) (adj c2_7 c2_6)
    (adj c3_6 c3_7) (adj c3_7 c3_6)
    (adj c4_6 c4_7) (adj c4_7 c4_6)
    (adj c5_6 c5_7) (adj c5_7 c5_6)

    ;; between y=7 and y=8
    (adj c1_7 c1_8) (adj c1_8 c1_7)
    (adj c2_7 c2_8) (adj c2_8 c2_7)
    (adj c3_7 c3_8) (adj c3_8 c3_7)
    (adj c4_7 c4_8) (adj c4_8 c4_7)
    (adj c5_7 c5_8) (adj c5_8 c5_7)

    ;; --- Door geometry (true 5x8 map) ---

    ;; Bedroom door at (3,4), connecting (3,4) <-> (3,5)
    (door-cell bedroom_door c3_4)
    (door-edge bedroom_door c3_4 c3_5)
    (door-edge bedroom_door c3_5 c3_4)

    ;; Kitchen door at (5,4), connecting (5,4) <-> (5,5)
    (door-cell kitchen_door c5_4)
    (door-edge kitchen_door c5_4 c5_5)
    (door-edge kitchen_door c5_5 c5_4)

    ;; Note: doors start CLOSED -> we do NOT assert (door-open ...)
    ;; Unlocking is done via unlock-door action when the cat has
    ;; the correct key.
  )

  ;; ----- Goal -----
  ;; For this template we set the goal as "cat reaches the mouse cell (1,8)".
  ;; When you integrate with Prolog, you will regenerate this part
  ;; so that the goal cell matches the *current* mouse position.
  (:goal
    (cat-at c1_8)
  )
)
