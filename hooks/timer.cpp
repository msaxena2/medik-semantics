#include <unistd.h>
#include <assert.h>
#include <vector>
#include <exception>
#include <iostream>
#include <event2/event.h>
#include "runtime/header.h"

#define get_header(name, symbol) \
static struct blockheader name() {\
  static struct blockheader hdr = {(uint64_t)-1}; \
  if (hdr.hdr == -1) { \
    hdr = getBlockHeaderForSymbol((uint64_t)getTagForSymbolName(symbol)); \
  } \
  return hdr; \
}

struct zinj {
  blockheader h;
  mpz_ptr data;
};


get_header(intHdr, "inj{SortInt{}, SortKItem{}}")

static event_base *base = event_base_new();
static int timer_count = 1;
static int pending = 0;

typedef struct event_id {
  struct timeval *interval;
  int id;
} event_id;

static std::vector<event_id *> timeout_events = std::vector<event_id *>();

void cb_func(evutil_socket_t fd, short what, void *arg) {
  event_id *tid = (event_id *) arg;
  pending--;
  timeout_events.push_back(tid);
}

extern "C" {
    set hook_SET_element(block*);

    const mpz_ptr hook_TIMER_sleep(mpz_ptr in) {
        int duration = mpz_get_si(in);
        struct timeval *interval = (struct timeval *)malloc(sizeof(struct timeval));
        interval->tv_sec = duration;
        interval->tv_usec = 0;
        event_id *tid = (event_id *)malloc(sizeof(struct event_id));
        tid->interval = interval;
        tid->id = timer_count++;
        struct event *ev = event_new(base, -1, EV_TIMEOUT, cb_func, tid);
        event_add(ev, tid->interval);
        pending++;
        mpz_t result;
        mpz_init_set_ui(result, static_cast<unsigned long>(tid->id));
        return move_int(result);
    }

    const set hook_TIMER_getTimeout() {
        auto tid_set = set();
        if(event_base_loop(base, EVLOOP_ONCE) != 0) {
          throw "dispatch error\n";
        }
        if(timeout_events.size() > 0) {
          for(auto ev : timeout_events) {
            mpz_t result;
            mpz_init_set_ui(result, static_cast<unsigned long>(ev->id));
            zinj *inj = (zinj *)koreAlloc(sizeof(zinj));
            inj->h = intHdr();
            inj->data = move_int(result);
            tid_set = tid_set.insert((SortKItem) inj);
          }
        }
        return tid_set;
    }
}
