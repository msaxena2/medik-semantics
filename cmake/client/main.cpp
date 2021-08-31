#include <iostream>
#include <regex>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <getopt.h>
#define CPPHTTPLIB_THREAD_POOL_COUNT 1
#include <httplib.h>
#include "k.h"
#include "runtime/alloc.h"
#include <gflags/gflags.h>

void runKServer(httplib::Server *svr);
void countBrackets(const char *buffer, size_t len);
bool doneReading (const char *buffer, int len);

static int K_DEPTH;
static int K_WRITE_FD;
static int K_READ_FD;

int brace_counter_, bracket_counter_, object_counter_;

DEFINE_int32(port, 8545, "Port to listen on with HTTP protocol");
DEFINE_int32(depth, -1, "For debugging, stop execution at a certain depth.");
DEFINE_string(host, "localhost", "IP/Hostname to bind to");
DEFINE_bool(dump, false, "Dump the K Server configuration on shutdown");
int main(int argc, char **argv) {
  gflags::ParseCommandLineFlags(&argc, &argv, true);

  K_DEPTH = FLAGS_depth;
  httplib::Server svr;

  // Start KServer in a separate thread
  std::thread t1([&] () {
    runKServer(&svr);
  });
  t1.detach();

  svr.Post(R"(.*)",
    [&](const httplib::Request &req, httplib::Response &res, const httplib::ContentReader &content_reader) {
      std::string body;
      content_reader([&](const char *data, size_t data_length) {
        countBrackets(data, data_length);
        body.append(data, data_length);
        return true;
      });

      write(K_WRITE_FD, body.c_str(), body.length());

      std::string message;
      char buffer[4096] = {0};
      int ret;

      do {
        ret = read(K_READ_FD, buffer, 4096);
        if (ret > 0) message.append(buffer, ret);
      } while (ret > 0 && !doneReading(buffer, ret));

      res.set_content(message, "application/json");
    });

  std::thread t2([&] () {
    svr.listen(FLAGS_host.c_str(), FLAGS_port);
  });

  t2.join();

  return 0;
}

void runKServer(httplib::Server *svr) {
  in_addr address;
  inet_aton("127.0.0.1", &address);

  static blockheader injHeaderInt      = getBlockHeaderForSymbol(getTagForSymbolName("inj{SortInt{}, SortKItem{}}"));
  static blockheader injHeaderCommands = getBlockHeaderForSymbol(getTagForSymbolName("inj{SortCommands{}, SortKItem{}}"));

  initStaticObjects();
  set_gc_interval(10000);

  int input[2], output[2];
  if (pipe(input)) {
    perror("input pipe");
    exit(1);
  }
  if (pipe(output)) {
    perror("output pipe");
    exit(1);
  }

  zinj *inputinj = (zinj *)koreAlloc(sizeof(zinj));
  inputinj->h = injHeaderInt;
  mpz_t input_z;
  mpz_init_set_si(input_z, input[0]);
  inputinj->data = move_int(input_z);
  K_WRITE_FD = input[1];

  zinj *outputinj = (zinj *)koreAlloc(sizeof(zinj));
  outputinj->h = injHeaderInt;
  mpz_t output_z;
  mpz_init_set_si(output_z, output[1]);
  outputinj->data = move_int(output_z);
  K_READ_FD = output[0];

  static uint64_t accept = (((uint64_t)getTagForSymbolName("Lblaccept{}")) << 32) | 1;
  inj *kinj = (inj *)koreAlloc(sizeof(inj));
  kinj->h = injHeaderCommands;
  kinj->data = (block *)accept;

  map withInput = hook_MAP_element(configvar("$INPUT"), (block *)inputinj);
  map withOutput = hook_MAP_update(&withInput, configvar("$OUTPUT"), (block *)outputinj);
  map init = hook_MAP_update(&withOutput, configvar("$PGM"), (block *)kinj);

  // invoke the rewriter
  static uint32_t tag2 = getTagForSymbolName("LblinitGeneratedTopCell{}");
  void *arr[1];
  arr[0] = &init;
  block* init_config = (block *)evaluateFunctionSymbol(tag2, arr);
  block* final_config = take_steps(K_DEPTH, init_config);
  if (FLAGS_dump) printConfiguration("/dev/stderr", final_config);
  close(K_WRITE_FD);
  close(K_READ_FD);
  svr->stop();
}

void bracketHelper(char c) {
  switch(c){
    case '{':{
      brace_counter_++;
      break;
    }
    case '}':{
      brace_counter_--;
      break;
    }
    case '[':{
      bracket_counter_++;
      break;
    }
    case ']':{
      bracket_counter_--;
      break;
    }
  }
}

void countBrackets(const char *buffer, size_t len) {
  for(int i = 0; i < len; i++) {
    bracketHelper(buffer[i]);
    if(0 == brace_counter_ && 0 == bracket_counter_) {
      object_counter_++;
    }
  }
}

bool doneReading (const char *buffer, int len) {
  for(int i = 0; i < len; i++){
    bracketHelper(buffer[i]);
    if(0 == brace_counter_ && 0 == bracket_counter_){
      object_counter_--;
    }
  }
  return 0 == brace_counter_
      && 0 == bracket_counter_
      && 0 == object_counter_;
}
