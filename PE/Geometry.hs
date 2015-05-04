module PE.Geometry where

data Point a = Point a a 

instance Show a => Show (Point a) where
    show (Point x y) = "(" ++ (show x) ++ ";" ++ (show y) ++ ")" 

data Triangle a = Triangle a a a

instance Show a => Show (Triangle a) where
    show (Triangle a b c) = show (a,b,c)

perimeter :: (Num a) => Triangle a -> a
perimeter t@(Triangle a b c) = a + b + c

coords (Triangle a b c) = ((Point 0 0),(Point c 0),(Point ad d))
    where cosa = (b^2 + c^2 - a^2) / (2*b*c)
          ad = b * cosa
          d = sqrt (b^2 - ad^2)
                        
incenter t@(Triangle a b c) =
    let ((Point xa ya),(Point xb yb),(Point xc yc)) = coords t
        p = perimeter t
    in Point ((a*xa + b*xb + c*xc) / p) ((a*ya + b*yb + c*yc) / p)

area t@(Triangle a b c) = sqrt $ p*(p - a)*(p - b)*(p - c)
    where p = perimeter t

cosines (Triangle a b c) = (cosa,cosb,cosc)
    where cosa = ((-sa) + sb + sc) / (2*b*c)
          cosb = (sa - sb + sc) / (2*a*c)
          cosc = (sa + sb - sc) / (2*a*b)
          sa = a^2
          sb = b^2
          sc = c^2

angles t = (arcA,arcB,arcC)
    where arcA = toDegrees a
          arcB = toDegrees b
          arcC = toDegrees c
          (a,b,c) = cosines t
          toDegrees x = (180/pi)*(acos x)

data Line a = Line (Point a) (Point a)

instance Show a => Show (Line a) where
    show (Line p1 p2) = show (p1,p2)

length (Line p1 p2) =
    let (Point x1 y1) = p1
        (Point x2 y2) = p2
        in sqrt $ ((x2 - x1)^2) + ((y2 - y1)^2)

slopeIntercept (Line p1 p2) = (slope,intercept)
    where (Point x1 y1) = p1
          (Point x2 y2) = p2
          slope = (y2 - y1) / (x2 - x1)
          intercept = (-slope) * x1 + y1

intersection l1 l2 = Point x y
    where (a,c) = slopeIntercept l1
          (b,d) = slopeIntercept l2
          x = (d - c) / (a - b)
          y = a * x + c

