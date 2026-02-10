#include <cpp11.hpp>
#include <fstream>
#include <vector>
#include <string>
#include <cstring>
#include <cstdint>

// Constants matching teems-solver/teems-parser/export_solution.c
#define NAMESIZE 256
#define TABREADLINE 20000
#define HEADERSIZE 5
#define MAXVARDIM 10
#define MAXSUPSET 12

typedef int uvdim;
typedef long int uvadd;
typedef double forreal;
typedef float ha_floattype;

// Struct definitions matching the solver binary format exactly

struct ha_cgeset {
  char header[HEADERSIZE];
  int fileid;
  char setname[NAMESIZE];
  char readele[TABREADLINE];
  uvadd begadd;
  uvdim size;
  uvdim subsetid[MAXSUPSET];
  bool intertemp;
  int intsup;
  bool regional;
  int regsup;
};

struct ha_cgesetele {
  char setele[NAMESIZE];
  uvdim setsh[MAXSUPSET];
};

struct hcge_cof {
  char cofname[NAMESIZE];
  uvadd begadd;
  uvdim size;
  uvadd setid[MAXVARDIM];
  uvadd antidims[MAXVARDIM];
  uvadd matsize;
  bool level_par;
  bool change_real;
  bool suplval;
  int gltype;
  ha_floattype glval;
};

[[cpp11::register]]
cpp11::list parse_solution_bins(std::string path_prefix) {

  // --- Read .mds file: 4 long ints ---
  std::string mds_path = path_prefix + "mds";
  std::ifstream mds_file(mds_path, std::ios::binary);
  if (!mds_file.is_open()) {
    cpp11::stop("Cannot open .mds file: %s", mds_path.c_str());
  }
  uvadd modeldes[4];
  mds_file.read(reinterpret_cast<char*>(modeldes), sizeof(uvadd) * 4);
  mds_file.close();

  uvadd nsetspace = modeldes[0];
  uvadd nvar      = modeldes[1];
  uvadd nvarele   = modeldes[2];
  uvadd nset      = modeldes[3];

  // --- Read .bin file: nvarele doubles ---
  std::string bin_path = path_prefix + "bin";
  std::ifstream bin_file(bin_path, std::ios::binary);
  if (!bin_file.is_open()) {
    cpp11::stop("Cannot open .bin file: %s", bin_path.c_str());
  }
  cpp11::writable::doubles bin_vec(static_cast<R_xlen_t>(nvarele));
  bin_file.read(reinterpret_cast<char*>(REAL(bin_vec)), sizeof(forreal) * nvarele);
  bin_file.close();

  // --- Read .var file: nvar hcge_cof structs ---
  std::string var_path = path_prefix + "var";
  std::ifstream var_file(var_path, std::ios::binary);
  if (!var_file.is_open()) {
    cpp11::stop("Cannot open .var file: %s", var_path.c_str());
  }
  std::vector<hcge_cof> var_structs(nvar);
  var_file.read(reinterpret_cast<char*>(var_structs.data()),
                sizeof(hcge_cof) * nvar);
  var_file.close();

  cpp11::writable::strings var_cofname(static_cast<R_xlen_t>(nvar));
  cpp11::writable::doubles var_begadd(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_size(static_cast<R_xlen_t>(nvar));
  cpp11::writable::doubles var_matsize(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_level_par(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_change_real(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_suplval(static_cast<R_xlen_t>(nvar));
  cpp11::writable::integers var_gltype(static_cast<R_xlen_t>(nvar));
  cpp11::writable::doubles var_glval(static_cast<R_xlen_t>(nvar));
  cpp11::writable::strings var_setid(static_cast<R_xlen_t>(nvar));
  cpp11::writable::strings var_antidims(static_cast<R_xlen_t>(nvar));

  for (long int i = 0; i < nvar; i++) {
    var_cofname[i] = std::string(var_structs[i].cofname);
    var_begadd[i]  = static_cast<double>(var_structs[i].begadd);
    var_size[i]    = var_structs[i].size;
    var_matsize[i] = static_cast<double>(var_structs[i].matsize);
    var_level_par[i]   = static_cast<int>(var_structs[i].level_par);
    var_change_real[i] = static_cast<int>(var_structs[i].change_real);
    var_suplval[i]     = static_cast<int>(var_structs[i].suplval);
    var_gltype[i]      = var_structs[i].gltype;
    var_glval[i]       = static_cast<double>(var_structs[i].glval);

    std::string sid;
    for (int k = 0; k < MAXVARDIM; k++) {
      if (k > 0) sid += ",";
      sid += std::to_string(var_structs[i].setid[k]);
    }
    var_setid[i] = sid;

    std::string adim;
    for (int k = 0; k < MAXVARDIM; k++) {
      if (k > 0) adim += ",";
      adim += std::to_string(var_structs[i].antidims[k]);
    }
    var_antidims[i] = adim;
  }

  using namespace cpp11::literals;
  cpp11::writable::list var_list(
    {"cofname"_nm = (SEXP)var_cofname,
     "begadd"_nm = (SEXP)var_begadd,
     "size"_nm = (SEXP)var_size,
     "setid"_nm = (SEXP)var_setid,
     "antidims"_nm = (SEXP)var_antidims,
     "matsize"_nm = (SEXP)var_matsize,
     "level_par"_nm = (SEXP)var_level_par,
     "change_real"_nm = (SEXP)var_change_real,
     "suplval"_nm = (SEXP)var_suplval,
     "gltype"_nm = (SEXP)var_gltype,
     "glval"_nm = (SEXP)var_glval}
  );

  // --- Read .sel file: nsetspace ha_cgesetele structs ---
  std::string sel_path = path_prefix + "sel";
  std::ifstream sel_file(sel_path, std::ios::binary);
  if (!sel_file.is_open()) {
    cpp11::stop("Cannot open .sel file: %s", sel_path.c_str());
  }
  std::vector<ha_cgesetele> sel_structs(nsetspace);
  sel_file.read(reinterpret_cast<char*>(sel_structs.data()),
                sizeof(ha_cgesetele) * nsetspace);
  sel_file.close();

  cpp11::writable::strings sel_setele(static_cast<R_xlen_t>(nsetspace));
  cpp11::writable::strings sel_setsh(static_cast<R_xlen_t>(nsetspace));

  for (long int i = 0; i < nsetspace; i++) {
    sel_setele[i] = std::string(sel_structs[i].setele);
    std::string sh;
    for (int k = 0; k < MAXSUPSET; k++) {
      if (k > 0) sh += ",";
      sh += std::to_string(sel_structs[i].setsh[k]);
    }
    sel_setsh[i] = sh;
  }

  cpp11::writable::list sel_list(
    {"setele"_nm = (SEXP)sel_setele,
     "setsh"_nm = (SEXP)sel_setsh}
  );

  // --- Read .set file: nset ha_cgeset structs ---
  std::string set_path = path_prefix + "set";
  std::ifstream set_file(set_path, std::ios::binary);
  if (!set_file.is_open()) {
    cpp11::stop("Cannot open .set file: %s", set_path.c_str());
  }
  std::vector<ha_cgeset> set_structs(nset);
  set_file.read(reinterpret_cast<char*>(set_structs.data()),
                sizeof(ha_cgeset) * nset);
  set_file.close();

  cpp11::writable::strings set_header(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_fileid(static_cast<R_xlen_t>(nset));
  cpp11::writable::strings set_setname(static_cast<R_xlen_t>(nset));
  cpp11::writable::strings set_readele(static_cast<R_xlen_t>(nset));
  cpp11::writable::doubles set_begadd(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_size(static_cast<R_xlen_t>(nset));
  cpp11::writable::strings set_subsetid(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_intertemp(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_intsup(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_regional(static_cast<R_xlen_t>(nset));
  cpp11::writable::integers set_regsup(static_cast<R_xlen_t>(nset));

  for (long int i = 0; i < nset; i++) {
    set_header[i]    = std::string(set_structs[i].header);
    set_fileid[i]    = set_structs[i].fileid;
    set_setname[i]   = std::string(set_structs[i].setname);
    set_readele[i]   = std::string(set_structs[i].readele);
    set_begadd[i]    = static_cast<double>(set_structs[i].begadd);
    set_size[i]      = set_structs[i].size;
    set_intertemp[i] = static_cast<int>(set_structs[i].intertemp);
    set_intsup[i]    = set_structs[i].intsup;
    set_regional[i]  = static_cast<int>(set_structs[i].regional);
    set_regsup[i]    = set_structs[i].regsup;

    std::string ssid;
    for (int k = 0; k < MAXSUPSET; k++) {
      if (k > 0) ssid += ",";
      ssid += std::to_string(set_structs[i].subsetid[k]);
    }
    set_subsetid[i] = ssid;
  }

  cpp11::writable::list set_list(
    {"header"_nm = (SEXP)set_header,
     "fileid"_nm = (SEXP)set_fileid,
     "setname"_nm = (SEXP)set_setname,
     "readele"_nm = (SEXP)set_readele,
     "begadd"_nm = (SEXP)set_begadd,
     "size"_nm = (SEXP)set_size,
     "subsetid"_nm = (SEXP)set_subsetid,
     "intertemp"_nm = (SEXP)set_intertemp,
     "intsup"_nm = (SEXP)set_intsup,
     "regional"_nm = (SEXP)set_regional,
     "regsup"_nm = (SEXP)set_regsup}
  );

  // --- Return combined list ---
  cpp11::writable::list result(
    {"bin"_nm = (SEXP)bin_vec,
     "var"_nm = (SEXP)var_list,
     "sel"_nm = (SEXP)sel_list,
     "set"_nm = (SEXP)set_list}
  );

  return result;
}
