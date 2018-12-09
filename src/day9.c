#include<stdio.h>
#include<stdlib.h>
#define NUM long int
#define SCORE long long int

typedef struct node node;

struct node {
  NUM val;
  node* next;
};

SCORE getHighscore(const int players, const NUM marbles) {
  SCORE* const scores = calloc(players,sizeof(NUM));
  node* const start = calloc(marbles,sizeof(node));
  node* temp;
  node* nextNode = start;
  node* curNode = nextNode++;
  curNode->val = 0;
  curNode->next = curNode;

  int behind = 0;
  node* trailing = curNode;

  int player = 0;

  for (NUM marble = 1; marble <= marbles; marble++) {
    if (marble % 23 == 0) {
      node* const removed = trailing->next;
      scores[player] += (SCORE) (marble + removed->val);

      curNode = removed->next;
      trailing->next = curNode;
      curNode = curNode->next;

      behind = 0;
      trailing = curNode;
    }
    else {
      node* const newNode = nextNode++;
      newNode->val = marble;
      newNode->next = curNode->next;

      curNode->next = newNode;
      curNode = newNode;

      curNode = curNode->next;
      trailing = trailing->next;

      if (behind <= 8)
        behind++;
      else
        trailing = trailing->next;
    }


    player = (player+1) % players;
  }

  SCORE maximum = 0;

  for (player = 0; player < players; player++)
    if (scores[player] > maximum)
      maximum = scores[player];

  free(scores);
  free(start);

  return maximum;
}

void solve(const int players, const NUM marbles) {
  printf("%d players\n%ld marbles\n%ld MiB allocated\n", players, marbles, (marbles*sizeof(node)+players*sizeof(NUM))/1024/1024);
  printf("highscore: %lld\n\n", getHighscore(players, marbles));
}

int main(int argc, char** argv) {
  solve(418, 71339);
  solve(418, 71339*100);
  return 0;
}
