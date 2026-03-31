#include "api.hpp"
#include <string>
#include <set>
#include <vector>
#include <queue>

std::string state_name(std::set<std::string>& s) {
	std::string res;
	for (auto& elem : s) res += elem;
	return res;
}

DFA dfa_minim(DFA &d) {
	std::string sigma = d.get_alphabet().to_string();
	std::set<std::string> all_states = d.get_states(), reachable, new_trans, trans, final, not_final;

	//delete unreachable and make complete
	d.create_state("deadlock");
	std::string q0 = d.get_initial_state();
	reachable.insert(q0);
	new_trans.insert(q0);
	all_states.erase(q0);
	while (!new_trans.empty()) {
		trans = new_trans;
		new_trans = std::set<std::string>();
		for (auto& r : trans) {
			for (auto a : sigma) {
				if (d.has_trans(r, a)) {
					std::string q = d.get_trans(r, a);
					if (reachable.find(q) == reachable.end()) {
						new_trans.insert(q);
						reachable.insert(q);
						all_states.erase(q);
					}
				}
				else {
					d.set_trans(r, a, "deadlock");
				}
			}
		}
	}
	for (auto a : sigma) d.set_trans("deadlock", a, "deadlock");
	for (auto& state : all_states) {
		d.delete_state(state);
	}
	
	std::vector<std::set<std::string>> partition;
	std::queue<std::set<std::string>> separators;
	for (auto& state : d.get_states()) {
		if (d.is_final(state)) final.insert(state);
		else not_final.insert(state);
	}
	if (!final.empty()) {
		partition.push_back(final);
		separators.push(final);
	}
	if (!not_final.empty()) {
		partition.push_back(not_final);
		separators.push(not_final);
	}
	while (!separators.empty()) {
		std::set<std::string> sep_class = separators.front();
		separators.pop();
		for (auto a : sigma) {
			std::vector<std::set<std::string>> new_partition, replace;
			for (auto& cur_class : partition) {
				std::set<std::string> part1, part2;
				for (auto& state : cur_class) {
					std::string to_state = d.get_trans(state, a);
					if (sep_class.find(to_state) != sep_class.end()) part1.insert(state);
					else part2.insert(state);
				}
				if (!part1.empty() && !part2.empty()) {
					replace.push_back(cur_class);
					new_partition.push_back(part1);
					new_partition.push_back(part2);
					separators.push(part1);
					separators.push(part2);
				}
			}
			for (auto& part : partition) {
				bool found = false;
				for (auto& r : replace) {
					if (part == r) {
						found = true;
						break;
					}
				}
				if (!found) new_partition.push_back(part);
			}
			partition = new_partition;
		}
	}

	//create minim dfa
	DFA min_d(sigma);
	for (auto& part : partition) {
		std::string name = state_name(part);
		min_d.create_state(name);
		if (part.find(q0) != part.end()) min_d.set_initial(name);
		if (d.is_final(*part.begin())) min_d.make_final(name);
	}
	for (auto& part : partition) {
		std::string name = state_name(part), one_state = *part.begin();
		for (auto a : sigma) {
			std::string to_state = d.get_trans(one_state, a);
			for (auto& to_class : partition) {
				if (to_class.find(to_state) != to_class.end()) {
					min_d.set_trans(name, a, state_name(to_class));
				}
			}
		}
	}

	//delete deadlocks
	std::set<std::string> not_dead, check;
	for (auto& state : min_d.get_states()) {
		if (min_d.is_final(state)) not_dead.insert(state);
		else check.insert(state);
	}
	size_t cur = check.size(), prev = 0;
	while (prev != cur) {
		prev = cur;
		std::set<std::string> cur_not_dead;
		for (auto& c : check) {
			for (auto a : sigma) {
				if (min_d.has_trans(c, a) && not_dead.find(min_d.get_trans(c, a)) != not_dead.end()) {
					not_dead.insert(c);
					cur_not_dead.insert(c);
					break;
				}
			}
		}	
		for (auto& c : cur_not_dead) check.erase(c);
		cur = check.size();
	}
	for (auto& c : check) min_d.delete_state(c);
	return min_d;
}
