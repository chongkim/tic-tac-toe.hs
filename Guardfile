require "guard/guard"

module ::Guard
  class Haskell < Guard
    def run_on_change(paths)
      paths.each do |path|
        run(path)
      end
    end
    def run_all
      Dir["spec/**Spec.hs"].each do |path|
        run(path)
      end
    end
    def run(path)
      cmd = "runhaskell #{path}"
      puts cmd
      system cmd
    end
  end
end

guard "haskell" do
  watch(%r{spec/.*\.hs})
  watch(%r{(TTT/.*)\.hs}) { |m,base| "spec/#{base}Spec.hs" }
end
