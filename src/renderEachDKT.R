library( ANTsR )
library( magick )
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
for ( wren  in  ) {
anatName = DesikanKillianyTourville$label_name[
  DesikanKillianyTourville$label_num == wren ]
print( anatName )
anatName2 = gsub( " ", "_", DesikanKillianyTourville$label_name[
  DesikanKillianyTourville$label_num == wren ] )
print( anatName )
################################
ctxseg = smoothImage( thresholdImage( imgg, wren, wren )  , 1 )
if ( DesikanKillianyTourville$tissue[
  DesikanKillianyTourville$label_num == wren ] == "Cortex" )
  ctxseg = smoothImage( thresholdImage( imgg, wren, wren ) * wm , 1 )
if ( max( ctxseg ) > 0 ) {
  ################################
  brain<-renderSurfaceFunction( surfimg =list( brn ) ,
     list( ctxseg ), alphasurf=0.00 ,smoothsval = 1.5,
     mycol='white', zoom = 0.89 )

  ridFront=matrix( c(0.9978505, 0.02926127, 0.05863563, 0,  0.05218979,  0.1862696,  -0.9811115,  0,  -0.03963061,  0.9820628,  0.1843421,  0,  0,  0,  0,  1), nrow=4)

  ridRight = matrix( c( 0.02866919, -0.01574883,  -0.9994649,  0,  -0.9935374,  0.1094071,  -0.03022304,  0,  0.1098246,  0.9938722,  -0.01251043,  0,  0,  0,  0,  1 ), nrow=4 )

  ridTop = matrix( c(0.9974232, 0.005292146, 0.07154182, 0, 0.02632965, -0.9546813, -0.2964625, 0, 0.06673081, 0.2975824, -0.952361, 0, 0, 0, 0, 1 ), nrow=4 )

  ifn = paste0( "~/Downloads/brainRenderInvi_", anatName2 )
  dd<-make3ViewPNG(  ridFront, ridRight, ridTop, ifn )
  ifn = paste0( ifn, '.png' )
  i1 = image_read( ifn )
  ixxxx = image_annotate( i1, as.character( anatName ), size = 20,
          color = "black", boxcolor = "white", degrees = 0, location = "+5+5")
  image_write( ixxxx, paste0( ifn ) )
  }
}




brain<-renderSurfaceFunction( surfimg =list( brn ) ,
   alphasurf=0.00 ,smoothsval = 1.5,
   mycol='white', zoom = 0.89 )
ifn = paste0( "~/Downloads/brainRenderInvi"  )
dd<-make3ViewPNG(  ridFront, ridRight, ridTop, ifn )
ifn = paste0( ifn, '.png' )
i1 = image_read( ifn )
ixxxx = image_annotate( i1, as.character( anatName ), size = 20,
        color = "black", boxcolor = "white", degrees = 0, location = "+5+5")
image_write( ixxxx, paste0( ifn ) )
