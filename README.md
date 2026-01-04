an attempt to translate bits of of CGAL into Haskell

currently available: halfedge graph [Euler Operations](https://doc.cgal.org/5.4.5/BGL/group__PkgBGLEulerOperations.html)

providing instances of [Class](src/Hgal/Graph/Class.hs) / [ClassM](src/Hgal/Graph/ClassM.hs)
allows for the use of [EulerOperations](src/Hgal/Graph/EulerOperations.hs) / [EulerOperationsM](src/Hgal/Graph/EulerOperationsM.hs)

[SurfaceMesh](src/Hgal/Data/SurfaceMesh.hs) is a naive halfedge graph implementation mostly for reference / testing purposes.
