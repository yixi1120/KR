(define (domain catmouse)
  (:requirements :strips :typing)

  ;; ----- Types -----
  (:types
    cell
    door
    key
  )

  ;; ----- Predicates -----
  (:predicates
    ;; positions
    (cat-at ?c - cell)
    (mouse-at ?c - cell)

    ;; adjacency inside rooms (not crossing doors)
    (adj ?from ?to - cell)

    ;; door geometry and state
    (door-cell ?d - door ?c - cell)        ; door is located at this cell
    (door-edge ?d - door ?from ?to - cell) ; crossing this edge uses the door
    (door-open ?d - door)                  ; door has been unlocked

    ;; keys and their pairing with doors
    (key-at ?k - key ?c - cell)            ; key lies on a cell
    (has-key ?k - key)                     ; cat is carrying this key
    (key-for ?k - key ?d - door)           ; which door this key opens
  )

  ;; ----- Actions -----

  ;; 1) Normal move inside rooms (no door crossing)
  (:action move-cat
    :parameters (?from ?to - cell)
    :precondition (and
      (cat-at ?from)
      (adj ?from ?to))
    :effect (and
      (not (cat-at ?from))
      (cat-at ?to))
  )

  ;; 2) Move through a door: only possible if that door is open
  (:action move-through-door
    :parameters (?d - door ?from ?to - cell)
    :precondition (and
      (cat-at ?from)
      (door-edge ?d ?from ?to)
      (door-open ?d))
    :effect (and
      (not (cat-at ?from))
      (cat-at ?to))
  )

  ;; 3) Pick up a key lying on the same cell as the cat
  (:action pickup-key
    :parameters (?k - key ?loc - cell)
    :precondition (and
      (cat-at ?loc)
      (key-at ?k ?loc))
    :effect (and
      (has-key ?k)
      (not (key-at ?k ?loc)))
  )

  ;; 4) Unlock a door while standing on its door-cell and holding the
  ;;    correct key for that door.
  (:action unlock-door
    :parameters (?d - door ?loc - cell ?k - key)
    :precondition (and
      (door-cell ?d ?loc)
      (cat-at ?loc)
      (has-key ?k)
      (key-for ?k ?d))
    :effect (door-open ?d)
  )
)
