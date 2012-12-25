(function() {
  require = function(path) {
    return _require(__ev8__.vm, __ev8__.current_script_name, path);
  };

  require.resolve = function(path) {
    return _resolve(__ev8__.current_script_name, path);
  };
})();
