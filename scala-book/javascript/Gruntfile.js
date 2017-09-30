'use strict';

module.exports = function (grunt) { //eslint-disable-line

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json')
  });

  grunt.initConfig({
    mochaTest: {
      unit: {
        options: {
          reporter: 'spec',
          quiet: false, // Optionally suppress output to standard out (defaults to false)
          clearRequireCache: false // Optionally clear the require cache before running tests (defaults to false)
        },
        src: ['spec/**/*.js']
      }
    },
    eslint: {
      target: ['**/*.js', '!node_modules/**/*.js', '!coverage/**/*.js']
    },
    watch: {
      test: {
        files: ['src/**/*.js', 'spec/**/*.js'],
        tasks: ['test']
      }
    }
  });

  // Load the plugin that provides the "uglify" task.
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-concurrent');
  grunt.loadNpmTasks('grunt-mocha-test');
  grunt.loadNpmTasks('grunt-nodemon');
  grunt.loadNpmTasks('grunt-eslint');
  grunt.loadNpmTasks('grunt-env');
  // Default task(s).
  grunt.registerTask('test', ['mochaTest:unit', 'eslint']);
  grunt.registerTask('watch-test', ['mochaTest:unit', 'watch:test']);
};
