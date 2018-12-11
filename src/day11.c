#include<stdio.h>

void main() {
  int powerLevel[301][301];
  int powerSum[301][301];
  int serial = 2694;

  for (int x = 1; x <= 300; x++) {
    for (int y = 1; y <= 300; y++) {
        int rackId = x+10;
        powerLevel[x][y] = ((((rackId*y+serial)*rackId) / 100) % 10)-5;
        powerSum[x][y] = 0;
      }
  }

  int pmax = -1;
  int xmax = -1;
  int ymax = -1;
  int smax = -1;

  for (int s = 1; s <= 300  ; s++)
  for (int x = 1; x <= 301-s; x++)
  for (int y = 1; y <= 301-s; y++) {
    int p = powerSum[x][y];
    for (int dx = 0; dx < s; dx++)
      p += powerLevel[x+dx][y+s-1];
    for (int dy = 0; dy < s-1; dy++)
      p += powerLevel[x+s-1][y+dy];
    powerSum[x][y] = p;

    if (p > pmax){
      if (smax == 3 && s > 3)
        printf("part 1: %d,%d\n", xmax, ymax);

      pmax = p;
      xmax = x;
      ymax = y;
      smax = s;
    }
  }

  printf("part 2: %d,%d,%d", xmax, ymax, smax);
}
