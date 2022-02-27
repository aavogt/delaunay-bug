{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
import Algorithms.Geometry.DelaunayTriangulation.Naive as Good
import Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer as Bad
import Data.Ext
import Data.Geometry
import qualified Data.List.NonEmpty as NE

import Control.DeepSeq
import Criterion.Main
import Algorithms.Geometry.DelaunayTriangulation.Types

-- Bad only works with Rational?
-- Good is faster with Double than Bad with Rational
-- why can I not build this project while ../ is able to find a good hgeometry?
psD :: NE.NonEmpty (Point 2 Double :+ ())
psD = fmap (\(a,b) -> Point2 (fromIntegral a) (fromIntegral b) :+ ()) $
   NE.fromList [(97,294),(97,287),(99,270),(101,265),(103,253),(104,249),(108,243),(112,236),(114,234),(116,231),(118,229),(119,228),(121,228),(122,228),(124,228),(128,229),(132,234),(139,242),(146,261),(150,277),(153,296),(153,306),(153,313),(152,320),(152,323),(151,326),(151,328),(151,329),(151,330),(151,331),(151,332),(151,332),(151,333),(33,261),(33,261),(34,253),(38,232),(42,212),(44,201),(49,189),(51,185),(52,182),(54,178),(56,176),(57,176),(58,174),(60,174),(61,174),(65,174),(68,175),(70,178),(74,186),(78,193),(82,208),(85,224),(87,239),(88,250),(88,261),(88,266),(87,270),(86,275),(86,278),(85,281),(84,283),(84,285),(83,286),(82,287),(82,288),(82,289),(82,290),(82,290),(82,291),(82,291)]
psR :: NE.NonEmpty (Point 2 Rational :+ ())
psR = fmap (\(a,b) -> Point2 (fromIntegral a) (fromIntegral b) :+ ()) $
   NE.fromList [(97,294),(97,287),(99,270),(101,265),(103,253),(104,249),(108,243),(112,236),(114,234),(116,231),(118,229),(119,228),(121,228),(122,228),(124,228),(128,229),(132,234),(139,242),(146,261),(150,277),(153,296),(153,306),(153,313),(152,320),(152,323),(151,326),(151,328),(151,329),(151,330),(151,331),(151,332),(151,332),(151,333),(33,261),(33,261),(34,253),(38,232),(42,212),(44,201),(49,189),(51,185),(52,182),(54,178),(56,176),(57,176),(58,174),(60,174),(61,174),(65,174),(68,175),(70,178),(74,186),(78,193),(82,208),(85,224),(87,239),(88,250),(88,261),(88,266),(87,270),(86,275),(86,278),(85,281),(84,283),(84,285),(83,286),(82,287),(82,288),(82,289),(82,290),(82,290),(82,291),(82,291)]

instance (NFData a, NFData b) => NFData (Triangulation a b) where
   rnf (Triangulation m v w) = rnf m `seq` rnf v `seq` rnf w

bd = Bad.delaunayTriangulation psD
br = Bad.delaunayTriangulation psR

main = defaultMain [bgroup "delaunay" [
     bench "Naive Double" $ nf Good.delaunayTriangulation psD,
     bench "Naive Rational" $ nf Good.delaunayTriangulation psR,
     bench "D&C Rational" $ nf Bad.delaunayTriangulation psR]]