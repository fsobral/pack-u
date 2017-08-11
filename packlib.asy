pen cor(int i) {

  if (i == 1)      return rgb(225 / 225.0,  77 / 225.0,  77 / 225.0);
  else if (i == 2) return rgb( 77 / 225.0, 112 / 225.0, 225 / 225.0);
  else if (i == 3) return rgb( 77 / 225.0, 225 / 225.0,  87 / 225.0);
  else if (i == 4) return rgb(160 / 225.0, 108 / 225.0, 224 / 225.0);

  return gray(0.5);

}

path caixa(pair p, real l, real w) {

  return shift(p) * box((0, 0), (l, w));
  
}

void drawBox(pair p, real l, real w, pen fp, pen dp, string name) {

  filldraw(caixa(p, l, w), fp, dp);
  
  label(name, shift(p) * ((l,w) / 2.0), dp);

}
