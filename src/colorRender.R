library( ANTsR )
library( magick )

ridFront=matrix( c(0.9978505, 0.02926127, 0.05863563, 0,  0.05218979,  0.1862696,  -0.9811115,  0,  -0.03963061,  0.9820628,  0.1843421,  0,  0,  0,  0,  1), nrow=4)

ridRight = matrix( c( 0.02866919, -0.01574883,  -0.9994649,  0,  -0.9935374,  0.1094071,  -0.03022304,  0,  0.1098246,  0.9938722,  -0.01251043,  0,  0,  0,  0,  1 ), nrow=4 )

ridTop = matrix( c(0.9974232, 0.005292146, 0.07154182, 0, 0.02632965, -0.9546813, -0.2964625, 0, 0.06673081, 0.2975824, -0.952361, 0, 0, 0, 0, 1 ), nrow=4 )


img=antsImageRead("antsMalfLabelingRight.nii.gz")
seg=antsImageRead("antsMalfLabeling_6LabelsRight.nii.gz")
wm = thresholdImage( seg, 4, 4 ) %>% morphology("dilate",0)
brn = thresholdImage( seg, 1, 4 ) %>% morphology("close",1) %>%
  iMath("FillHoles")  %>% iMath("GetLargestComponent") %>% smoothImage( 1.5 )
imgg = iMath( img, "GD", 1 )
################################
data( DesikanKillianyTourville )
ctx = DesikanKillianyTourville$label_num[
  DesikanKillianyTourville$tissue == 'Cortex']
################################
wren = 116
wren = 102
olList = list()
ct = 1
wrens = sample( na.omit( DesikanKillianyTourville$label_num[ DesikanKillianyTourville$hemisphere == "Right" ] ) )[1:11]
for ( wren  in wrens )
  {
  anatName = DesikanKillianyTourville$label_name[
    DesikanKillianyTourville$label_num == wren ]
  anatName2 = gsub( " ", "_", DesikanKillianyTourville$label_name[
    DesikanKillianyTourville$label_num == wren ] )
  print( anatName )
  ################################
  ctxseg = smoothImage( thresholdImage( imgg, wren, wren )  , 1 )
  if ( DesikanKillianyTourville$tissue[
    DesikanKillianyTourville$label_num == wren ] == "Cortex" )
    ctxseg = smoothImage( thresholdImage( imgg, wren, wren ) * wm , 1 )
  if ( max( ctxseg ) > 0 ) {
    olList[[ ct ]] = ctxseg
    ct = ct + 1
    }
  }

library( viridis )
vircols = inferno( length( olList ) )
brain<-renderSurfaceFunction( surfimg =list( brn ) ,
     olList, alphasurf=0.15 ,smoothsval = 1.5,
     mycol = vircols, zoom = 0.89 )

ifn = paste0( "~/Downloads/brainRenderMULT"  )
dd<-make3ViewPNG(  ridFront, ridRight, ridTop, ifn )
