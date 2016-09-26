data Tree : Type -> Type -> Type where
  Empty : Tree k v
  Node : k -> v -> Tree k v -> Tree k v -> Tree k v

insert : Tree k v -> k -> v -> Tree k v

search : Tree k v -> k -> Maybe v

delete : Tree k v -> k -> Tree k v

map : (v -> v') -> Tree k v -> Tree k v'
