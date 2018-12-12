#include<iostream>
#include<unordered_set>
#include<algorithm>


#define NUM long int
#define OFFSET long int
#define C(p) (p ? '#' : '.')

using namespace std;


NUM simulate(const NUM generations, const unordered_set<int> rules, const unordered_set<OFFSET> start_state) {
  unordered_set<OFFSET>* gen_cur = new unordered_set<OFFSET>(start_state);
  unordered_set<OFFSET>* gen_new;
  NUM checkShiftAt = 1;

  for (NUM generation = 0; generation < generations; generation++) {
    gen_new = new unordered_set<OFFSET>();

    for (const auto plant : *gen_cur) {
      //rule_acc is the number representing the plants around plant+dx (???.#[#]..??? -> 01100b -> rule 12)
      int rule_acc = 0;
      for (OFFSET dx = -4; dx < 0; dx++)
        rule_acc = (rule_acc*2)+gen_cur->count(plant+dx);

      for (OFFSET dx = -2; dx <= 2; dx++) {
        rule_acc = ((rule_acc*2) % 32)+gen_cur->count(plant+dx+2);

        if (rules.count(rule_acc))
          gen_new->insert(plant+dx);
      }
    }

#ifdef DEBUG
    for (OFFSET x = -3; x < 35; x++)
      cout << C(gen_new->count(x));
    cout << endl;
#endif

    //sometimes check if the new state is simply a shift of the previous one
    if (generation == checkShiftAt) {
      checkShiftAt *= 2;

      if (gen_cur->size() == gen_new->size()) {
        OFFSET first_cur = *min_element(gen_cur->begin(), gen_cur->end());
        OFFSET first_new = *min_element(gen_new->begin(), gen_new->end());
        OFFSET diff = first_new-first_cur;
        if (all_of(gen_cur->begin(), gen_cur->end(), [gen_new,diff](OFFSET plant){return gen_new->count(plant+diff);})) {
          delete gen_new;
          gen_new = new unordered_set<OFFSET>();

          diff *= generations-generation;
          for (const auto plant : *gen_cur)
            gen_new->insert(plant+diff);

          generation = generations;
        }
      }
    }

    delete gen_cur;
    gen_cur = gen_new;
  }

  NUM sum = 0;
  for (const auto plant : *gen_cur)
    sum += plant;

  return sum;
}


void solve(const NUM generations, const unordered_set<int> rules, const unordered_set<OFFSET> start_state) {
#ifdef DEBUG
  cout << "generations: " << generations << endl;

  cout << "rules:" << endl;
  for (int r = 0; r < 32; r++)
    cout << C(r&16) << C(r&8) << C(r&4) << C(r&2) << C(r&1) << ": " << C(rules.count(r)) << endl;
  cout << endl;

  cout << "initial state:" << endl;
  for (OFFSET x = 0; x < 100; x++)
    cout << C(start_state.count(x));
  cout << endl;
#endif

  NUM sum = simulate(generations, rules, start_state);

  cout << "plant sum: " << sum << endl;
}


int main() {
  //rules encoded as a set of numbers (#..## -> 10011b -> 19) that produce a plant
  //state encoded as a set of offsets that have a plant

#ifdef DEBUG
  unordered_set<int> example_rules({3, 4, 8, 10, 11, 12, 15, 21, 23, 26, 27, 28, 29, 30});
  unordered_set<OFFSET> example_start_state({0, 3, 5, 8, 9, 16, 17, 18, 22, 23, 24});
  solve(20, example_rules, example_start_state);
#endif

  unordered_set<int> rules({2, 4, 5, 7, 9, 10, 11, 12, 13, 17, 22, 24, 26, 29, 30});
  unordered_set<OFFSET> start_state({
    0, 5, 6, 8, 10, 12, 13, 14,
    15, 18, 20, 21, 22, 23, 24, 25,
    28, 29, 31, 33, 34, 35, 36, 37,
    38, 39, 40, 43, 47, 48, 52, 53,
    57, 58, 60, 62, 66, 67, 68, 69,
    70, 71, 73, 74, 75, 80, 84, 85,
    88, 90, 95, 96, 98, 99
  });

  solve(20, rules, start_state);
  solve(50000000000, rules, start_state);
}
