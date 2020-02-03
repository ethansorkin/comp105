functor DictFn (structure Key : KEY) :> DICT where type key = Key.key = 
struct
    type key = Key.key
    type 'a dict = LEAF
                 | NODE of 'a dict * key * 'a * 'a dict

    exception NotFound of key

    val empty = nil
    val find(k, NODE (left, key, value, right)) = case Key.compare(k, key) 
                                                   of EQUAL => value
                                                    | LESS => find (k, left)
                                                    | GREATER => find (k, right) 
                     
      | find(k, LEAF) = raise NotFound k
    val bind = 
end