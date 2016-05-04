module V = Lacaml_S.Vec

let mapi f =
  let cnt = ref 0 in
  let mapper x:float = let i = !cnt in cnt := i + 1; f i x in
  V.map mapper
