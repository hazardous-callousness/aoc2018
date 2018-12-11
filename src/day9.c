#include<stdio.h>
#include<stdlib.h>
#define NODE unsigned int
#define SCORE long

SCORE get_winning_score(const int players, const NODE marbles) {
  SCORE* const scores = calloc(players,sizeof(SCORE));
  NODE*  const after  = calloc(marbles,sizeof(NODE));
  NODE current = 0, back;
  after[current] = current;

  for (NODE marble = 1; marble <= marbles; marble++, current = after[current]) {
    if (marble % 23 == 0) {
      const NODE removed = after[back], next = after[removed];
      after[back] = next;
      current     = next;

      const int player = marble % players;
      scores[player] += (SCORE)marble + (SCORE)removed;
    }
    else {
      const NODE next = after[current], new = marble;
      after[current] = new;
      after[new]     = next;
      current        = new;

      if (marble % 23 == 18)
        back = current;
    }
  }

  SCORE maximum = 0;
  for (int player = 0; player < players; player++)
    if (scores[player] > maximum)
      maximum = scores[player];

  free(scores);
  free(after);

  return maximum;
}

void solve(const int players, const NODE marbles) {
  const int mem = (marbles*sizeof(NODE)+players*sizeof(SCORE))/1024/1024;
  printf("%u players\n%u marbles\n%u MiB allocated\n", players, marbles, mem);
  printf("highscore: %ld\n\n", get_winning_score(players, marbles));
}

int main(int argc, char** argv) {
  solve(9,25);
  solve(10,1618);
  // solve(418, 71339);
  return 0;
}
