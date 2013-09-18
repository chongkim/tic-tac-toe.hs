require "guard/guard"

module ::Guard
  class Haskell < Guard
    def initialize(watchers=[], options={})
      @options = options
      super(watchers, options)
    end
    def run_on_change(paths)
      paths.each do |path|
        run(path)
      end
    end
    def run_on_additions(paths)
      run_on_change(paths)
    end
    def start
      run_all
    end
    def run_all
      Dir["spec/**/*[Ss]pec.*"].each do |path|
        run(path)
      end
    end
    def run(path)
      cmd = "echo ----------------------------------------\nrunhaskell"
      cmd << " #{@options[:cli]}" if @options[:cli]
      cmd << " #{path}"
      puts cmd
      system cmd
    end
  end
end

guard "haskell" do
  watch(%r{^spec/.*spec.*$}i)
  watch(%r{^(TTT/.*)\.([^./]+)$}) { |m| "spec/#{m[1]}_spec.#{m[2]}" }
end

