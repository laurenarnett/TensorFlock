main = invert 255 img;

img : T<rows, cols, ncolors>;
img = #tensors/pixl.tensor;

invert : Nat -> T<rows, cols, ncolors> -> T<rows, cols, ncolors>;
invert colordepth t = cast colordepth - t[i,j,k];
