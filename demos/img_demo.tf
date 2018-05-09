main = invert img;

img : T<4,4,3>;
img = #tensors/pixl.tensor;

invert : T<4,4,3> -> T<4,4,3>;
invert t = 255. - t[i,j,k];
