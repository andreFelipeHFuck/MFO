#include <assert.h>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <unordered_map>

#include "bank.hpp"

using json = nlohmann::json;
using namespace std;

enum class Action {
  Init,
  Deposit,
  Withdraw,
  Transfer,
  BuyInvestment,
  SellInvestment,
  Unknown
};

Action stringToAction(const std::string &actionStr) {
  static const std::unordered_map<std::string, Action> actionMap = {
      {"init", Action::Init},
      {"deposit_action", Action::Deposit},
      {"withdraw_action", Action::Withdraw},
      {"transfer_action", Action::Transfer},
      {"buy_investment_action", Action::BuyInvestment},
      {"sell_investment_action", Action::SellInvestment}};

  auto it = actionMap.find(actionStr);
  if (it != actionMap.end()) {
    return it->second;
  } else {
    return Action::Unknown;
  }
}

int int_from_json(json j) {
  string s = j["#bigint"];
  return stoi(s);
}

map<string, int> balances_from_json(json j) {
  map<string, int> m;
  for (auto it : j["#map"]) {
    m[it[0]] = int_from_json(it[1]);
  }
  return m;
}

map<int, Investment> investments_from_json(json j) {
  map<int, Investment> m;
  for (auto it : j["#map"]) {
    m[int_from_json(it[0])] = {.owner = it[1]["owner"],
                               .amount = int_from_json(it[1]["amount"])};
  }
  return m;
}

BankState bank_state_from_json(json state) {
  map<string, int> balances = balances_from_json(state["balances"]);
  map<int, Investment> investments =
      investments_from_json(state["investments"]);
  int next_id = int_from_json(state["next_id"]);
  return {.balances = balances, .investments = investments, .next_id = next_id};
}


void print_bank_state(BankState &bank_state) {
  cout << "Balances: " << endl;
  for (auto [k, v]: bank_state.balances) {
      cout << "\t" << k << "->" << v << endl;
  }

  cout << "Investments: " << endl;
  for (auto [k, v]: bank_state.investments) {
      cout << "\t" << k << "-> {" << endl;
      cout << "\t\t" << "Owner: " << v.owner << endl;
      cout << "\t\t" << "Amount: " << v.amount << endl;
  }

  cout << "Next ID: " << bank_state.next_id << endl;
}



int main() {
  for (int i = 0; i < 10000; i++) {
    cout << "Trace #" << i << endl;
    std::ifstream f("traces/out" + to_string(i) + ".itf.json");
    json data = json::parse(f);

    // Estado inicial: começamos do mesmo estado incial do trace
    BankState bank_state =
        bank_state_from_json(data["states"][0]["bank_state"]);

    auto states = data["states"];
    for (auto state : states) {
      string action = state["action_taken"];
      json nondet_picks = state["nondet_picks"];

      string error = "";

      // Próxima transição
      switch (stringToAction(action)) {
      case Action::Init: {
        cout << "initializing" << endl;
        break;
      }
      case Action::Deposit: {
        string depositor = nondet_picks["depositor"]["value"];
        int amount = int_from_json(nondet_picks["amount"]["value"]);
        cout << "deposit_action(" << state << "," << depositor << "," << amount << ")" << endl;
        error = deposit(bank_state, depositor, amount);
        break;
      }
      default: {
        cout << endl;
        cout << "TODO: fazer a conexão para as outras ações. Ação: " << action
             << endl;
        cout << endl;
        error = "";
        break;
      }
      }

      BankState expected_bank_state = bank_state_from_json(state["bank_state"]);
      string expected_error = string(state["error"]["tag"]).compare("Some") == 0
                                  ? state["error"]["value"]
                                  : "";

      cout << "-------------------- Expected --------------------------------"
           << endl;

      print_bank_state(expected_bank_state);
      cout << "Error: " << expected_error << endl;

      cout << "-------------------- Actual ----------------------------------"
           << endl;
      
      print_bank_state(bank_state);
      cout << "Error: " << error << endl;

      cout << "--------------------------------------------------------------"
           << endl;
      cout << endl;

      // assert(state == expected_bank_state);
      assert(error == expected_error);


    }
  }
  return 0;
}
