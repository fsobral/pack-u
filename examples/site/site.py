import web
import time
from packu import packureduce as pr
from packu import packu as prr


urls = (
    '/', 'Seller',
    '/calc', 'Calculator'
)


render = web.template.render('templates', base='base')


class Item:

    def __init__(self, n):

        self.name = n


class Seller:

    def GET(self):

        items = pr.parse_items_files('items.txt')

        return render.products(items)


class Calculator:

    def GET(self):

        inp = web.input(data=None)

        numberOfItems = []

        for it in inp.data.split(','):

            quant = 0

            try:

                quant = int(it)

            except Exception:

                pass

            numberOfItems.append(quant)

        # Generates data file and runs

        startTime = time.perf_counter()

        with open("data.txt", "w") as fp:

            for it in numberOfItems:

                fp.write("{0:05d}\n".format(it))

        prr.runPacku()

        endTime = time.perf_counter()

        # Returns results

        frete = 0.0

        with open("solution.csv", "r") as fp:

            for l in fp:

                frete += 14.80

        return render.frete("R$ {0:10.2f}".format(frete),
                            "{0:5.2f}".format(endTime - startTime))


app = web.application(urls, globals())

if __name__ == '__main__':

    app.run()
