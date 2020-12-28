module Types where
data Node = Node Int 
           |NoNode
           
data Link x = Link {
           a    :: Node,
           cost :: x,
           b    :: Node}
data Path a = Path a (Link a) (Path a)
            | EndOfPath
                  
