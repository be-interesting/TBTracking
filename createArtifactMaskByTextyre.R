# assume bg

a <- glcm(bg, n_grey=10)

kern <- makeBrush(23, shape='disc')
b <- dilateGreyScale(a[,,5]>0.9, kern)
c <- erodeGreyScale(b, kern)

display(c)
