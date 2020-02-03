(class Creature Object
    [alive]

    (class-method new () (birth (new super)))
    (method birth () (set alive true) self)
    (method alive? () alive)
    (method kill () (set alive false) self)
    (method eats: (c) (and: alive {eatshelper c}))
    (method eatshelper: (c) (subclassResponsibility self))
)

(class Chicken Creature
    []
    (method eatshelper: (c) (chickenEats: c))
    (method eatenByChicken:(creature))
)

(class Fly Creature
    []

    (method eatenByFly: ())
)

(class FlyTrap Creature
    []

    (method eatenByFlyTrap: ())
)