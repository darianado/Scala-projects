// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

import scala.annotation.tailrec

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

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

@tailrec
def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    if(path.length == dim*dim) Some(path)
    else 
        tour_on_mega_board(dim,ordered_moves(dim,path,path.head).head::path)
}

}
//tour_on_mega_board(70, List((0, 0)))