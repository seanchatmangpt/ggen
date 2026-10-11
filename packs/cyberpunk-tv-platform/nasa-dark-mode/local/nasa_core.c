#include <stdint.h>

/*
 * Dependency-free bounded state transition core for NASA Dark Mode.
 * This body has no I/O authority. It transforms admitted state only.
 */

enum {
  KEY_LEFT = 0,
  KEY_RIGHT = 1,
  KEY_UP = 2,
  KEY_DOWN = 3,
  KEY_OK = 4,
  KEY_BACK = 5
};

enum {
  OP_REFUSED = 0,
  OP_PREVIOUS_MODE = 1,
  OP_NEXT_MODE = 2,
  OP_PREVIOUS_MISSION = 3,
  OP_NEXT_MISSION = 4,
  OP_SELECT_INTENT = 5,
  OP_PRIVACY_CURTAIN = 6
};

__attribute__((export_name("policy_version")))
uint32_t policy_version(void) {
  return 1u;
}

/*
 * Packed result layout:
 * bits  0..3   mode index
 * bits  4..11  mission index + 1 (-1 is encoded as 0)
 * bit   12     privacy curtain
 * bit   13     admitted standing (1 ALIVE, 0 REFUSED)
 * bits 14..17  operation code
 */
__attribute__((export_name("apply_remote")))
uint32_t apply_remote(int32_t mode, int32_t mission, int32_t mission_count,
                      int32_t privacy, int32_t key) {
  uint32_t standing = 1u;
  uint32_t operation = OP_REFUSED;

  if (key == KEY_LEFT) {
    operation = OP_PREVIOUS_MODE;
    mode = (mode + 3) & 3;
  } else if (key == KEY_RIGHT) {
    operation = OP_NEXT_MODE;
    mode = (mode + 1) & 3;
  } else if (key == KEY_UP) {
    operation = OP_PREVIOUS_MISSION;
    if (mission_count > 0) mission = (mission - 1 + mission_count) % mission_count;
  } else if (key == KEY_DOWN) {
    operation = OP_NEXT_MISSION;
    if (mission_count > 0) mission = (mission + 1) % mission_count;
  } else if (key == KEY_OK) {
    operation = OP_SELECT_INTENT;
  } else if (key == KEY_BACK) {
    operation = OP_PRIVACY_CURTAIN;
    privacy = privacy ? 0 : 1;
  } else {
    standing = 0u;
    operation = OP_REFUSED;
  }

  uint32_t encoded_mission = (uint32_t)(mission + 1) & 0xffu;
  return ((uint32_t)mode & 0x0fu)
       | (encoded_mission << 4)
       | (((uint32_t)privacy & 1u) << 12)
       | ((standing & 1u) << 13)
       | ((operation & 0x0fu) << 14);
}
