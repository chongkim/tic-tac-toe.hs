require "guard/guard"

module ::Guard
  class Haskell < Guard
    def run_on_change(paths)
      paths.each do |path|
        run(path)
      end
    end
    def run_all
      Dir["spec/**.hs"].each do |path|
        run(path)
      end
    end
    def run(path)
      puts("runhaskell -ilib #{path}")
      system("runhaskell -ilib #{path}")
    end
  end
end

guard "haskell" do
  watch(%r{^lib/(.*)\.hs}) { |m,f| "spec/#{f}_spec.hs" }
  watch(%r{^spec/.*\.hs})
end
