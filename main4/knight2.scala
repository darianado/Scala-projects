// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  !path.contains(x) &&
          x._1>=0 && x._1<dim && 
          x._2>=0 && x._2<dim
}

 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  List( 
    (x._1 +1 , x._2 +2),
    (x._1 +2 , x._2 +1),
    (x._1 +2 , x._2 -1),
    (x._1 +1 , x._2 -2),
    (x._1 -1 , x._2 -2),
    (x._1 -2 , x._2 -1),
    (x._1 -2 , x._2 +1),
    (x._1 -1 , x._2 +2),
  ).filter(x => is_legal(dim,path,x))
}


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    legal_moves(dim,path,x).sortBy(legal_moves(dim,path,_).length)
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  xs match{
    case Nil => None
    case x::xs =>{
      val fdx = f(x)
      if(fdx.isDefined) fdx
      else first(xs,f)
    }
  }
} 

def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if(path.length == dim*dim)
        if(ordered_moves(dim,List(),path.head).contains(path.last)) Some(path)
        else None
    else first(ordered_moves(dim,path,path.head), x=> first_closed_tour_heuristics(dim,x::path))
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if(path.length == dim*dim) Some(path)
    else 
        first(ordered_moves(dim,path,path.head), x=> first_tour_heuristics(dim,x::path))
}



}
